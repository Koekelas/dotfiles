;;; exar.el --- Emacs, X Window System and RandR -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Nicolas De Jaeghere

;; Author: Nicolas De Jaeghere <nicolas@dejaeghe.re>
;; Keywords: unix
;; URL: https://github.com/Koekelas/dotfiles

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Emacs, X Window System and RandR

;;; Code:

(require 'subr-x)
(require 'seq)
(require 'eieio)
(require 'xcb)
(require 'xcb-xproto)
(require 'xcb-randr)
(require 'exwm-core)
(require 'exwm-randr)

(defvar exar-monitors nil)
(defvar exar-layouts nil)

(defvar exar-manufacturers
  '(("AUO" . "AU Optronics Corp.")
    ("MJI" . "Marantz Japan Inc.")))

(defconst exar--x:True 1)
(defconst exar--x:False 0)
(defconst exar--x:AnyPropertyType 0)

(defvar exar--cookie nil)
(defvar exar--previous-layout nil)

(defun exar--plist-to-alist (plist)
  (mapcar (pcase-lambda (`(,name ,prop))
            (cons name prop))
          (seq-partition plist 2)))

(defun exar--get-in (plist &rest keys)
  (if keys
      (apply #'exar--get-in (plist-get plist (car keys)) (cdr keys))
    plist))

(defun exar--reverse-endian (bytes)
  (reverse bytes))

(defun exar--format-bin (bytes)
  (concat "0x" (mapconcat (apply-partially #'format "%02x") bytes "")))

;; Four byte ints, eight bit bytes, big endian
(defun exar--decode-int (bytes)
  (let ((base (expt 2 8)))
    (thread-last (exar--reverse-endian bytes)
      (seq-map-indexed (lambda (byte i)
                         (* byte (expt base i))))
      (apply #'+))))

(defun exar--decode-string (bytes)
  (decode-coding-string (apply #'unibyte-string bytes) 'utf-8))

(defun exar--decode-terminated-string (bytes)
  (exar--decode-string
   (seq-take-while (lambda (byte)
                     (/= byte ?\C-j))
                   bytes)))

(defun exar--decode-manufacturer (bytes)
  (let ((base (expt 2 5))
        (n (exar--decode-int bytes))
        (chars ()))
    (while (> n 0)
      (push (+ (1- ?A) (% n base)) chars)
      (setq n (/ n base)))
    (concat chars)))

(defun exar--decode-edid (bytes)
  (when bytes
    ;; http://read.pudn.com/downloads110/ebook/456020/E-EDID%20Standard.pdf
    (let ((vendor-block   (seq-subseq bytes 8 18))
          (edid-block     (seq-subseq bytes 18 20))
          (basic-block    (seq-subseq bytes 20 25))
          (detailed-block (seq-subseq bytes 54 126)))
      (list
       :id (exar--format-bin vendor-block)
       :vendor   ; Section 3.4, Vendor/product identification
       (let ((week (car (seq-subseq vendor-block 8 9))))
         (list
          :manufacturer
          (exar--decode-manufacturer (seq-subseq vendor-block 0 2))
          :product
          (exar--format-bin (exar--reverse-endian (seq-subseq vendor-block 2 4)))
          :serial
          (exar--decode-int (exar--reverse-endian (seq-subseq vendor-block 4 8)))
          :week (when (not (= week 0))
                  week)
          :year (+ (car (seq-subseq vendor-block 9 10)) 1990)))
       :edid     ; Section 3.5, EDID structure version/revision
       (list :version  (car edid-block) :revision (cadr edid-block))
       :basic    ; Section 3.6, Basic display parameters/features
       (let ((horizontal (car (seq-subseq basic-block 1 2)))
             (vertical   (car (seq-subseq basic-block 2 3))))
         (list
          :horizontal (and (not (or (= horizontal 0) (= vertical 0))) horizontal)
          :vertical   (and (not (or (= horizontal 0) (= vertical 0))) vertical)
          :gamma      (/ (+ (car (seq-subseq basic-block 3 4)) 100) 100.0)))
       :detailed ; Section 3.10, Detailed timing descriptions
       (mapcar
        (lambda (detailed)
          (let ((data (seq-drop detailed 5)))
            (pcase (exar--decode-int (seq-subseq detailed 0 4))
              (255
               (list
                :type "serial"
                :data (string-trim (exar--decode-terminated-string data))))
              (254
               (list
                :type "string"
                :data (string-trim (exar--decode-terminated-string data))))
              (253
               (list :type "range"))
              (252
               (list
                :type "name"
                :data (string-trim (exar--decode-terminated-string data))))
              (251
               (list :type "color"))
              (250
               (list :type "timing"))
              ((pred (lambda (marker)
                       (and (<= marker 249) (>= marker 17))))
               (list :type nil))
              (16
               (list :type "dummy"))
              ((pred (lambda (marker)
                       (<= marker 15)))
               (list :type "manufacturer" :data (exar--format-bin data)))
              (_type
               (list :type "timing")))))
        (seq-partition detailed-block 18))))))

(defun exar--intern-atom (name)
  (oref (xcb:+request-unchecked+reply exwm--connection
            (xcb:InternAtom :only-if-exists exar--x:False
                            :name-len (length name)
                            :name name))
        atom))

(defun exar--get-output-property (output-id name)
  (oref (xcb:+request-unchecked+reply exwm--connection
            (xcb:randr:GetOutputProperty :output output-id
                                         :property (exar--intern-atom name)
                                         :type exar--x:AnyPropertyType
                                         :long-offset 0
                                         :long-length 100
                                         :delete exar--x:False
                                         :pending exar--x:False))
        data))

(defun exar--get-output-info (output-id)
  (with-slots (name connection)
      (xcb:+request-unchecked+reply exwm--connection
          (xcb:randr:GetOutputInfo :output output-id
                                   :config-timestamp exar--cookie))
    (list :id output-id
          :name (exar--decode-string name)
          :edid (exar--decode-edid (exar--get-output-property output-id "EDID"))
          :connected (eq connection xcb:randr:Connection:Connected))))

(defun exar--get-outputs ()
  (with-slots (config-timestamp outputs)
      (xcb:+request-unchecked+reply exwm--connection
          ;; Read output properties (e.g. EDID)
          (xcb:randr:GetScreenResources :window exwm--root))
    (let ((exar--cookie config-timestamp))
      (mapcar (lambda (id)
                (cons id (exar--get-output-info id)))
              outputs))))

(defun exar--get-connected-outputs (outputs)
  (seq-filter (lambda (output)
                (plist-get (cdr output) :connected))
              outputs))

(defun exar--get-monitors (outputs)
  (let ((output-ids
         (seq-reduce (pcase-lambda (ids `(,id . ,props))
                       (when-let ((edid (exar--get-in props :edid :id)))
                         (push (cons edid id) ids))
                       ids)
                     outputs ())))
    (mapcar (pcase-lambda (`(,id . ,props))
              (cons id
                    (append (list :id id) props
                            (list :output
                                  (thread-first (plist-get props :edid)
                                    (assoc output-ids)
                                    cdr
                                    (alist-get outputs))))))
            (exar--plist-to-alist exar-monitors))))

(defun exar--get-layouts (outputs)
  (let ((monitors (exar--get-monitors outputs)))
    (mapcar
     (lambda (layout)
       (plist-put (copy-sequence layout)
                  :monitors
                  (mapcar (pcase-lambda (`(,id . ,props))
                            (append props (alist-get id monitors)))
                          (exar--plist-to-alist (plist-get layout :monitors)))))
     exar-layouts)))

(defun exar--get-preferred-layout (layouts)
  (seq-find (lambda (layout)
              (seq-every-p (lambda (monitor)
                             (plist-get monitor :output))
                           (plist-get layout :monitors)))
            layouts))

(defun exar--set-primary-output (output-id)
  (xcb:+request exwm--connection
      (xcb:randr:SetOutputPrimary :window exwm--root
                                  :output output-id)))

(defun exar--set-primary (monitor-layout)
  (exar--set-primary-output (exar--get-in (car monitor-layout) :output :id)))

(defun exar--set-workspaces (monitor-layout)
  (setq exwm-randr-workspace-monitor-plist
        (seq-mapcat (lambda (monitor)
                      (let ((output (exar--get-in monitor :output :name)))
                        (seq-mapcat (lambda (workspace)
                                      (list workspace output))
                                    (plist-get monitor :workspaces))))
                    monitor-layout)))

(defun exar--get-display-ns (outputs monitor-layout)
  (let* ((primary
          (assq (exar--get-in (car monitor-layout) :output :id) outputs))
         ;; Sort primary first, mirror dispwin
         (sorted (cons primary
                       (seq-filter (lambda (output)
                                     (/= (car output) (car primary)))
                                   outputs))))
    (seq-map-indexed (lambda (output i)
                       (cons (car output) (1+ i)))
                     sorted)))

(defun exar--load-color (display-n file-name)
  (when (fboundp 'exar--load-color-dispwin)
    (exar--load-color-dispwin display-n file-name)))

(when (executable-find "dispwin")
  (defun exar--load-color-dispwin (display-n file-name)
    (make-process
     :name "dispwin"
     :command `("dispwin" "-d" ,(number-to-string display-n) ,file-name))))

(defun exar--load-colors (display-ns monitor-layout)
  (dolist (monitor monitor-layout)
    (when-let ((color (plist-get monitor :color)))
      (exar--load-color (alist-get (exar--get-in monitor :output :id)
                                   display-ns)
                        color))))

(defun exar--apply ()
  (let* ((outputs (exar--get-outputs))
         (layout (exar--get-preferred-layout (exar--get-layouts outputs)))
         (monitor-layout (plist-get layout :monitors)))
    (when (and (not (string= (plist-get layout :name)
                             (plist-get exar--previous-layout :name)))
               monitor-layout)
      (exar--set-primary monitor-layout)
      (exar--set-workspaces monitor-layout)
      (exar--load-colors (exar--get-display-ns outputs monitor-layout)
                         monitor-layout)
      (setq exar--previous-layout layout))))

(defun exar-insert-edid (output)
  (interactive
   (list
    (let ((candidates
           (mapcar
            (lambda (output)
              (let* ((props (cdr output))
                     (manufacturer-name
                      (exar--get-in props :edid :vendor :manufacturer))
                     (monitor-names
                      (thread-last (exar--get-in props :edid :detailed)
                        (seq-filter (lambda (detailed)
                                      (let ((type (plist-get detailed :type)))
                                        (or (string= type "name")
                                            (string= type "string")))))
                        (mapcar (lambda (detailed)
                                  (plist-get detailed :data)))))
                     (output-name (plist-get props :name)))
                (cons
                 (concat (or (cdr (assoc manufacturer-name exar-manufacturers))
                             manufacturer-name)
                         (when monitor-names
                           (concat " " (string-join monitor-names " ")))
                         (concat " connected to " output-name))
                 output)))
            (exar--get-connected-outputs (exar--get-outputs)))))
      (cdr (assoc (completing-read "Monitor: " candidates nil t) candidates)))))
  (insert (exar--get-in (cdr output) :edid :id)))

;;;###autoload
(defun exar-enable ()
  (add-hook 'exwm-randr-screen-change-hook #'exar--apply)
  (exwm-randr-enable))

(provide 'exar)

;;; exar.el ends here
