;;; koek-ml --- Custom mode line -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2023 Nicolas De Jaeghere

;; Author: Nicolas De Jaeghere <nicolas@dejaeghe.re>
;; Keywords: frames
;; URL: <https://github.com/Koekelas/dotfiles>

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

;; This package provides koek-ml-mode, a global minor mode configuring
;; a custom mode line, for use with Nicolas' Emacs configuration.

;;; Code:

(eval-when-compile
  (require 'subr-x)
  (require 'ediff-init)
  (require 'pdf-macs))
(require 'seq)
(require 'koek-subr)
(require 'moody)

;;; Custom

(defgroup koek-ml nil
  "Custom mode line."
  :group 'frames)

(defface koek-ml/selected-workspace '((t . (:inherit mode-line-emphasis)))
  "Face for selected workspace label in mode line.")

(defface koek-ml/unselected-workspace nil
  "Face for unselected workspace label in mode line.")

(defface koek-ml/variant '((t . (:inherit mode-line-emphasis)))
  "Face for variant label in mode line.")

;;; Utility variables and functions

(defvar koek-ml/separator (make-string 3 ?\s)
  "Mode line group separator.")

(defvar koek-ml/large-separator
  (make-string (* (length koek-ml/separator) 5) ?\s)
  "Mode line left right separator.")

(defun koek-ml/strip-percent-constructs (s)
  "Strip percent constructs from string S.
S is a string, the string to strip percent constructs from."
  (thread-last
    s
    (replace-regexp-in-string (rx "%" (not "%")) "" s)
    (string-replace "%%" "%")))

;;; Constructs

;;;; Dummies

(defvar koek-ml/dummies '((eldoc-mode-line-string nil))
  "Mode line construct for dummies.
A dummy prevents a package from altering the mode line.")

;;;; eldoc

(defvar koek-ml/eldoc
  '(eldoc-mode-line-string ("" eldoc-mode-line-string koek-ml/separator))
  "Mode line construct for eldoc.")
(put 'koek-ml/eldoc 'risky-local-variable t)

;;;; ace-window

(defun koek-ml/prevent-setup-ace (f &rest args)
  "Prevent ace-window from altering the mode line.
Intended as advice around `ace-window-display-mode'."
  (let ((format (default-value 'mode-line-format)))
    (apply f args)
    (setq-default mode-line-format format)
    (force-mode-line-update 'all)))

(defun koek-ml/get-window-label ()
  "Return window label of selected window."
  (when-let ((label (window-parameter nil 'ace-window-path)))
    (substring-no-properties label)))

(defvar koek-ml/ace
  '(:eval
    (when (bound-and-true-p ace-window-mode)
      (when-let ((label (koek-ml/get-window-label)))
        `(,(moody-ribbon (propertize label 'face 'aw-mode-line-face) nil 'up)
          koek-ml/separator))))
  "Mode line construct for ace-window.")
(put 'koek-ml/ace 'risky-local-variable t)

;;;; ediff

(defvar-local koek-ml/variant nil
  "Ediff variant.")

(defvar koek-ml/ediff
  '(:eval
    (when (and koek-ml/variant (not (bound-and-true-p ace-window-mode)))
      `(,(let ((label (plist-get koek-ml/variant :label))
               (state (plist-get koek-ml/variant :state)))
           (moody-ribbon (concat (propertize label 'face 'koek-ml/variant)
                                 (when state " ")
                                 state)
                         nil 'up))
        koek-ml/separator)))
  "Mode line construct for ediff.")
(put 'koek-ml/ediff 'risky-local-variable t)

;;;; Recursive edit depth

(defvar koek-ml/depth
  '(:eval
    (let ((depth (- (recursion-depth) (minibuffer-depth))))
      (when (and (> depth 0) (moody-window-active-p))
        `(,(moody-ribbon (format "[%d]" depth) nil 'up)
          koek-ml/separator))))
  "Mode line construct for recursive edit depth.")
(put 'koek-ml/depth 'risky-local-variable t)

;;;; exwm workspaces

(defvar exwm-workspace--workareas)
(defvar exwm-workspace-current-index)

(defun koek-ml/get-exwm-workspaces ()
  "Return workspaces of selected monitor."
  (thread-last
    (number-sequence 0 (1- (length exwm-workspace--workareas)))
    (seq-group-by (lambda (n)
                    (nth n exwm-workspace--workareas)))
    (mapcar #'cdr)
    (seq-find (lambda (ns)
                (memq exwm-workspace-current-index ns)))
    (mapcar (lambda (n)
              (list :n n :label (or (koek-subr/arabic-to-roman n) "N"))))))

(defvar koek-ml/exwm-workspaces
  '(:eval
    (when (and (featurep 'exwm-workspace) (moody-window-active-p))
      (let ((workspaces (koek-ml/get-exwm-workspaces)))
        (when (length> workspaces 1)
          `(,(moody-ribbon
              (mapconcat
               (lambda (workspace)
                 (let ((face (if (= (plist-get workspace :n)
                                    exwm-workspace-current-index)
                                 'koek-ml/selected-workspace
                               'koek-ml/unselected-workspace)))
                   (propertize (plist-get workspace :label) 'face face)))
               workspaces " ")
              nil 'up)
            koek-ml/separator)))))
  "Mode line construct for exwm workspaces.")
(put 'koek-ml/exwm-workspaces 'risky-local-variable t)

;;;; Identification

(defvar koek-ml/id
  '(:eval
    (let ((prefix (when (and (featurep 'project)
                             (derived-mode-p 'prog-mode 'conf-mode))
                    (koek-proj/get-name))))
      (moody-tab
       (concat
        (when prefix (koek-subr/elide prefix 16))
        (when prefix "/")
        (propertize
         (koek-subr/elide (buffer-name) 32) 'face 'mode-line-buffer-id)))))
  "Mode line construct for identification.")
(put 'koek-ml/id 'risky-local-variable t)

;;;; State

(defvar koek-ml/state '(" " "%*%+")
  "Mode line construct for state.")

;;;; keycast

(defvar keycast-mode-line)

(defvar koek-ml/keycast
  '(:eval
    (when (bound-and-true-p keycast-mode)
      keycast-mode-line))
  "Mode line construct for keycast.")
(put 'koek-ml/keycast 'risky-local-variable t)

;;;; Position

(defvar koek-ml/position
  '(:eval
    (unless (derived-mode-p 'pdf-view-mode)
      `("" koek-ml/large-separator
        ,(when (buffer-narrowed-p)
           (list (moody-ribbon "Narrowed" nil 'up) " "))
        "%p" " " "%l,%c")))
  "Mode line construct for position.")
(put 'koek-ml/position 'risky-local-variable t)

;;;; pdf-tools

(declare-function pdf-cache-number-of-pages "ext:pdf-cache")

(defvar koek-ml/pdf
  '(:eval
    (when (derived-mode-p 'pdf-view-mode)
      `("" koek-ml/large-separator
        ,(format "%d/%d" (pdf-view-current-page) (pdf-cache-number-of-pages)))))
  "Mode line construct for pdf-tools.")
(put 'koek-ml/pdf 'risky-local-variable t)

;;;; exwm input

(defvar exwm--input-mode)

(defun koek-ml/prevent-update-exwm-input (f &rest args)
  "Prevent exwm from altering mode line process.
Intended as advice around `exwm-input--update-mode-line'."
  (let ((status mode-line-process))
    (apply f args)
    (setq mode-line-process status)
    (force-mode-line-update)))

(defvar koek-ml/exwm-input
  '(:eval
    (when (and (featurep 'exwm-core) (eq exwm--input-mode 'char-mode)
               (moody-window-active-p))
      `("" koek-ml/separator
        ,(moody-ribbon "Char" nil 'up))))
  "Mode line construct for exwm input.")
(put 'koek-ml/exwm-input 'risky-local-variable t)

;;;; Input

(defvar koek-ml/input
  '(:eval
    (when (and current-input-method (moody-window-active-p))
      `("" koek-ml/separator
        ,(moody-ribbon current-input-method-title nil 'up))))
  "Mode line construct for input.")
(put 'koek-ml/input 'risky-local-variable t)

;;;; flymake

(declare-function flymake--lookup-type-property "flymake")
(declare-function flymake-diagnostic-type "flymake")
(declare-function flymake-diagnostics "flymake")
(declare-function flymake-disabled-backends "flymake")
(declare-function flymake-reporting-backends "flymake")
(declare-function flymake-running-backends "flymake")

(defvar koek-ml/flymake-levels '(:error :warning :note))

(defvar koek-ml/checker-names
  '((eglot-flymake-backend      . "LSP")
    (elisp-flymake-byte-compile . "El")
    (elisp-flymake-checkdoc     . "CDoc")
    (flymake-kondor-backend     . "Kondo"))
  "Alist of checker symbol to checker name pairs.")

(defun koek-ml/get-flymake-state ()
  "Return state of flymake.
State is the symbol running (some checkers running),
finished (all checkers finished running), disabled (all
compatible checkers disabled) or no-checker (no compatible
checkers)."
  (let* ((enabled (flymake-running-backends))
         (finished (flymake-reporting-backends))
         (running (seq-difference enabled finished))
         (disabled (flymake-disabled-backends)))
    (cond
     (running
      'running)
     (finished
      'finished)
     (disabled
      'disabled)
     (t
      'no-checker))))

(defun koek-ml/state-to-description (state)
  "Convert flymake state STATE to a description.
STATE is a symbol, a flymake state."
  (let ((words (split-string (symbol-name state) "-")))
    (string-join (cons (capitalize (car words)) (cdr words)) " ")))

(defun koek-ml/get-flymake-n-diags ()
  "Return number of diagnoses per level."
  (thread-last
    (flymake-diagnostics)
    (seq-group-by #'flymake-diagnostic-type)
    (mapcar (pcase-lambda (`(,level . ,diags))
              (cons level (length diags))))))

(defvar koek-ml/flymake
  '(:eval
    (when (and (bound-and-true-p flymake-mode) (moody-window-active-p))
      `("" koek-ml/separator
        ,(when-let ((name
                     ;; First enabled checker
                     (alist-get (car (last (flymake-running-backends)))
                                koek-ml/checker-names)))
           (concat name " "))
        ,(pcase (koek-ml/get-flymake-state)
           ((or 'running 'finished)
            (let ((n-diags (koek-ml/get-flymake-n-diags)))
              (mapconcat
               (lambda (level)
                 (propertize
                  (number-to-string (alist-get level n-diags 0))
                  'face (flymake--lookup-type-property level 'mode-line-face)))
               koek-ml/flymake-levels ";")))
           (state
            (koek-ml/state-to-description state))))))
  "Mode line construct for flymake.")
(put 'koek-ml/flymake 'risky-local-variable t)

;;;; vc

(defvar vc-mode)

(defvar koek-ml/vc
  '(:eval
    (when (and (bound-and-true-p vc-mode) (moody-window-active-p))
      (let ((state (string-trim (substring-no-properties vc-mode))))
        ;; For format, see `vc-default-mode-line-string'
        (string-match
         (rx (group-n 1 (one-or-more (not (any "-:@!?")))) (any "-:@!?")
             (zero-or-one (one-or-more (not ":")) ":")
             (group-n 2 (one-or-more not-newline)))
         state)
        `("" koek-ml/separator
          ,(format "%s %s" (match-string 1 state) (match-string 2 state))))))
  "Mode line construct for version control.")
(put 'koek-ml/vc 'risky-local-variable t)

;;;; Task

(declare-function org-clock-get-clocked-time "org-clock")
(declare-function org-clocking-buffer "org")
(declare-function org-duration-from-minutes "org-duration")

(defvar koek-ml/task
  '(:eval
    (when (and (featurep 'org) (org-clocking-buffer) (moody-window-active-p))
      `("" koek-ml/separator
        ,(org-duration-from-minutes (org-clock-get-clocked-time)))))
  "Mode line construct for task.")
(put 'koek-ml/task 'risky-local-variable t)

;;;; Modes

(defvar koek-ml/modes
  '("" koek-ml/separator "(" mode-name mode-line-process minor-mode-alist ")")
  "Mode line construct for modes.")
(put 'koek-ml/modes 'risky-local-variable t)

;;; Presets

;;;; Default

(defvar koek-ml/mode-line-format
  `(,@koek-ml/dummies " "
    koek-ml/eldoc koek-ml/ace koek-ml/ediff
    koek-ml/depth koek-ml/exwm-workspaces
    koek-ml/id koek-ml/state keycast-marker
    koek-ml/position koek-ml/pdf koek-ml/exwm-input koek-ml/input
    koek-ml/flymake koek-ml/vc koek-ml/task koek-ml/modes))

;;;; transient

(defvar koek-ml/transient-format
  '(" "
    (:eval
     (moody-tab
      (propertize (koek-subr/construct-earmuffed-name
                   "transient" mode-line-buffer-identification)
                  'face 'mode-line-buffer-id)))
    koek-ml/large-separator
    koek-ml/modes))
(put 'koek-ml/transient-format 'risky-local-variable t)

;;;; ediff

(defvar koek-ml/variant-types '(A B C Ancestor)
  "List of variant types.")

(defun koek-ml/get-variant-state (type)
  "Return state of variant type TYPE for current diff.
TYPE is a symbol, the type of the variant, see
`koek-ml/variant-types'."
  (when (ediff-valid-difference-p)
    (let* ((diff
            (let ((diff
                   (if (eq type 'Ancestor)
                       (ediff-get-state-of-merge ediff-current-difference)
                     (ediff-get-state-of-diff ediff-current-difference type))))
              (pcase diff
                ((or "=diff(A)" "prefer-B") "=A")
                ((or "=diff(B)" "prefer-A") "=B")
                ("=diff(C)"                 "=C")
                ("=diff(A+B)"               "=A+B"))))
           (merge (when (eq type 'C)
                    (ediff-get-state-of-merge ediff-current-difference)))
           (ancestor
            (when (and (eq type 'C)
                       (ediff-get-state-of-ancestor ediff-current-difference)
                   "empty")))
           (state (concat diff
                          (when (and diff merge) ";")
                          merge
                          (when (and (or diff merge) ancestor) ";")
                          ancestor)))
      (unless (string-empty-p state)
        state))))

(defvar koek-ml/diff
  '(:eval
    `("" koek-ml/large-separator
      ,(let ((diff-n (1+ ediff-current-difference))
             (n-diffs ediff-number-of-differences))
         (cond
          ((< diff-n 1)
           (format "Begin -/%d" n-diffs))
          ((> diff-n n-diffs)
           (format "End -/%d" n-diffs))
          (t
           (format "%d/%d" diff-n n-diffs))))))
  "Mode line construct for ediff diff.")
(put 'koek-ml/diff 'risky-local-variable t)

(defun koek-ml/update-ediff ()
  "Update mode line of control buffer.
Intended as advice overriding `ediff-refresh-mode-lines'."
  (setq mode-line-format
        `(,@koek-ml/dummies " "
          koek-ml/eldoc koek-ml/ace
          koek-ml/depth koek-ml/exwm-workspaces
          koek-ml/id koek-ml/keycast
          koek-ml/diff koek-ml/task koek-ml/modes))
  (force-mode-line-update)
  (dolist (type koek-ml/variant-types)
    (when-let ((buffer (ediff-get-buffer type)))
      (let ((state (koek-ml/get-variant-state type)))
        (with-current-buffer buffer
          (setq koek-ml/variant
                (list :label (if (eq type 'Ancestor) "Anc" (symbol-name type))
                      :state state))
          (force-mode-line-update))))))

(defun koek-ml/cleanup-variants ()
  "Cleanup variants."
  (dolist (type koek-ml/variant-types)
    (when-let ((buffer (ediff-get-buffer type)))
      (with-current-buffer buffer
        (kill-local-variable 'koek-ml/variant)
        (force-mode-line-update)))))

;;;; calendar

(defvar calendar-buffer)
(declare-function calendar-current-date "calendar")
(declare-function calendar-date-string "calendar")
(declare-function calendar-set-mode-line "calendar")

(defun koek-ml/setup-calendar (description)
  "Setup mode line of calendar buffer.
Intended as advice overriding `calendar-set-mode-line'."
  (let ((normalized (koek-ml/strip-percent-constructs description)))
    (setq mode-line-format
          `(,@koek-ml/dummies " "
            koek-ml/eldoc koek-ml/ace
            koek-ml/depth koek-ml/exwm-workspaces
            koek-ml/id koek-ml/keycast
            ("" koek-ml/large-separator ,normalized)
            koek-ml/task koek-ml/modes)))
  (force-mode-line-update))

(defun koek-ml/update-calendar ()
  "Update mode line of calendar buffer.
Intended as advice overriding `calendar-update-mode-line'."
  ;; Calendar buffer isn't guaranteed to exist or be current
  (when-let ((buffer (get-buffer calendar-buffer)))
    (with-current-buffer buffer
      (calendar-set-mode-line
       (calendar-date-string (calendar-current-date)
                             'abbreviate 'no-dayname)))))

;;; Minor mode

(defvar koek-ml/old-mode-line-format nil)

;;;###autoload
(define-minor-mode koek-ml-mode
  "Custom mode line."
  :global t
  (if koek-ml-mode
      (progn
        (let ((format (default-value 'mode-line-format)))
          (unless (eq format koek-ml/mode-line-format)
            (setq koek-ml/old-mode-line-format format)))
        (setq-default mode-line-format koek-ml/mode-line-format)
        (advice-add 'ace-window-display-mode :around #'koek-ml/prevent-setup-ace)
        (advice-add 'exwm-input--update-mode-line :around #'koek-ml/prevent-update-exwm-input)
        (advice-add 'ediff-refresh-mode-lines :override #'koek-ml/update-ediff)
        (add-hook 'ediff-cleanup-hook #'koek-ml/cleanup-variants)
        (advice-add 'calendar-set-mode-line :override #'koek-ml/setup-calendar)
        (advice-add 'calendar-update-mode-line :override #'koek-ml/update-calendar))
    (setq-default mode-line-format koek-ml/old-mode-line-format)
    (setq koek-ml/old-mode-line-format nil)
    (advice-remove 'ace-window-display-mode #'koek-ml/prevent-setup-ace)
    (advice-remove 'exwm-input--update-mode-line #'koek-ml/prevent-update-exwm-input)
    (advice-remove 'ediff-refresh-mode-lines #'koek-ml/update-ediff)
    (remove-hook 'ediff-cleanup-hook #'koek-ml/cleanup-variants)
    (advice-remove 'calendar-set-mode-line #'koek-ml/setup-calendar)
    (advice-remove 'calendar-update-mode-line #'koek-ml/update-calendar))
  (force-mode-line-update 'all))

(provide 'koek-ml)

;;; koek-ml ends here
