;;; init.el --- Nicolas' Emacs configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2021 Nicolas De Jaeghere

;; Author: Nicolas De Jaeghere <nicolas@dejaeghe.re>
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

;; This file contains Nicolas' Emacs configuration.

;;; Code:

(require 'subr-x)
(require 'seq)
(require 'xdg)

(require 'nsm)

(setq nsm-settings-file
      (expand-file-name "var/nsm-settings.el" user-emacs-directory))
(make-directory (file-name-directory nsm-settings-file) 'parents)

(setq network-security-level 'high)

(defvar bootstrap-version)              ; Must be a dynamic variable

(setq straight-check-for-modifications '(check-on-save find-when-checking))

(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el"
                         user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(defmacro koek-pkg/register (package)
  "Register embedded package.
PACKAGE is a symbol, the package name."
  (declare (indent 1))
  (let ((package-name (symbol-name package)))
    `(progn
       ;; Emacs loads packages during compilation to warn about
       ;; undefined symbols and to expand macros
       (eval-and-compile
         (add-to-list 'load-path
                      ,(thread-last user-emacs-directory
                         (expand-file-name "lisp/")
                         (expand-file-name package-name))))
       (load ,(concat package-name "-autoloads.el")
             'noerror 'nomessage 'nosuffix))))

(straight-use-package 'no-littering)
(require 'no-littering)

(straight-use-package 'delight)         ; Optional dependency
(straight-use-package 'use-package)
(require 'use-package)

(defun koek-up/process-koek (package _keyword arg rem state)
  "Process :koek keyword.
PACKAGE is a symbol, the package name.  ARG is a symbol, the
keyword argument, either t or nil.  REM is a plist, the remaining
keywords.  STATE is a plist, the state of the keywords.  For more
information, see `use-package-process-keywords'."
  (use-package-concat (when arg
                        `((koek-pkg/register ,package)))
                      (use-package-process-keywords package rem state)))

(defalias 'use-package-normalize/:koek #'use-package-normalize-predicate)
(defalias 'use-package-handler/:koek #'koek-up/process-koek)

(let ((i (seq-position use-package-keywords :load-path)))
  (setq use-package-keywords
        (append (seq-subseq use-package-keywords 0 i)
                '(:koek)
                (seq-subseq use-package-keywords i))))

(straight-use-package
 `(org
   :pre-build
   ,(list (if (eq system-type 'berkeley-unix) "gmake" "make")
          "autoloads" "info"
          (concat "EMACS=" invocation-directory invocation-name))
   :build (:not autoloads info)))

(use-package gcmh
  :straight t
  :hook (after-init . gcmh-mode)
  :config
  (setq gcmh-high-cons-threshold (* (expt 1024 2) 16)) ; In bytes
  :delight)

;; Prevent exwm from asking to replace window manager after
;; installation
(setq exwm-replace nil)

(use-package exwm
  :straight t
  :when (string-equal (getenv "XDG_CURRENT_DESKTOP") "EXWM")
  :preface
  (defun koek-wm/get-process-args (id)
    "Return arguments of process id ID.
ID is an integer, the process id of the process."
    (when-let ((args (alist-get 'args (process-attributes id))))
      (let ((normalized
             (thread-last args
               (replace-regexp-in-string (rx "\\ ") "\N{NO-BREAK SPACE}")
               (replace-regexp-in-string (rx (one-or-more " ")) "\N{NULL}")
               (replace-regexp-in-string "\N{NO-BREAK SPACE}" " "))))
        (split-string normalized "\N{NULL}"))))

  (defun koek-wm/get-process-ids (name)
    "Return process ids of process NAME.
NAME is a string, the name of the process."
    (seq-filter (lambda (id)
                  (when-let ((args (koek-wm/get-process-args id)))
                    (let ((nm (thread-first args
                                car
                                (split-string "/")
                                last
                                car)))
                      (string-equal nm name))))
     (list-system-processes)))

  (defun koek-wm/set-xsettingsd-preset ()
    "Set xsettingsd configuration preset.
When current theme is a dark theme, set configuration preset to
dark, else, set it to light."
    (when-let ((id (car (koek-wm/get-process-ids "xsettingsd"))))
      (let* ((preset (if (koek-thm/darkp) "dark" "light"))
             (preset-file
              (thread-last (xdg-data-home)
                (expand-file-name "xsettingsd/presets/")
                (expand-file-name preset)))
             (config-file
              (expand-file-name "xsettingsd/xsettingsd" (xdg-config-home))))
        ;; Third argument truthy overwrites existing link, docstring
        ;; only mentions not signaling an error
        (make-symbolic-link preset-file config-file 'overwrite)
        (signal-process id 'SIGHUP))))

  ;; For systemctl power management commands, see
  ;; https://access.redhat.com/documentation/en-us/red_hat_enterprise_linux/8/html/configuring_basic_system_settings/managing-services-with-systemd_configuring-basic-system-settings#shutting-down-suspending-hibernating-system_managing-services-with-systemd
  (defun koek-wm/power-off ()
    "Power off system."
    (call-process "systemctl" nil 0 nil "poweroff"))

  (defun koek-wm/reboot ()
    "Reboot system."
    (call-process "systemctl" nil 0 nil "reboot"))

  ;; make-process
  (defun koek-wm/kill-power-off (&optional arg)
    "Kill Emacs and power off system.
With `\\[universal-argument]' prefix argument ARG, reboot
system."
    (interactive "P")
    (let ((kill-emacs-hook              ; Dynamic variable
           (append kill-emacs-hook
                   (list (if arg #'koek-wm/reboot #'koek-wm/power-off)))))
      (save-buffers-kill-terminal)))

  (defun koek-wm/suspend ()
    "Suspend system."
    (interactive)
    (call-process "systemctl" nil 0 nil "suspend"))
  :config
  ;; Only when package is loaded
  (bind-keys
   ("C-c z p" . koek-wm/kill-power-off)
   ("C-c z z" . koek-wm/suspend))

  (add-hook 'koek-thm/enable-hook #'koek-wm/set-xsettingsd-preset))

(use-package exwm-input
  :defer t
  :preface
  ;; The S modifier isn't recognized on the left. DEL isn't recognized
  ;; but <backspace> is.
  (defvar koek-wm/base-simulation-keys
    '(("C-f" . "<right>")
      ("C-b" . "<left>")
      ("C-n" . "<down>")
      ("C-p" . "<up>")
      ("M-f" . "C-<right>")
      ("M-b" . "C-<left>")
      ("C-e" . "<end>")
      ("C-a" . "<home>")
      ("C-v" . "<next>")
      ("M-v" . "<prior>")
      ("M->" . "C-<end>")
      ("M-<" . "C-<home>")
      ("C-s" . "C-f")
      ("C-d" . "<delete>")
      ("M-d" . "C-<delete>")
      ("M-<backspace>" . "C-<backspace>")
      ("C-k" . "S-<end> <backspace>")
      ("M-@" . "S-C-<right>")
      ("M-h" . "C-a")
      ("M-w" . "C-c")
      ("C-w" . "C-x")
      ("C-y" . "C-v")
      ("C-/" . "C-z")
      ("M-/" . "C-y"))
    "Alist of Emacs keybinding to non Emacs keybinding pairs.
Keybinding is a string, see `edmacro-mode'.")

  (define-advice exwm-input--update-mode-line
      (:around (f &rest args) koek-wm/disable-update-process-status)
    (let ((status mode-line-process))
      (apply f args)
      (setq mode-line-process status)
      (force-mode-line-update)))
  :config
  ;; Keybindings in exwm and non exwm buffers, even in char mode,
  ;; i.e., keybindings mustn't conflict with non Emacs keybindings
  ;; (e.g. copy, cut and paste). Keybindings associated with desktop
  ;; environments (e.g. maximize window, close window and switch
  ;; between windows) meet these requirements.
  (setq exwm-input-global-keys
        (mapcar (pcase-lambda (`(,key . ,command))
                  (cons (kbd key) command))
                '(("s-z" . repeat)
                  ("s-0" . koek-wm/switch-workspace-0)
                  ("s-1" . koek-wm/switch-workspace-1)
                  ("s-2" . koek-wm/switch-workspace-2)
                  ("s-3" . koek-wm/switch-workspace-3)
                  ("s-4" . koek-wm/switch-workspace-4)
                  ("s-5" . koek-wm/switch-workspace-5)
                  ("s-6" . koek-wm/switch-workspace-6)
                  ("s-7" . koek-wm/switch-workspace-7)
                  ("s-8" . koek-wm/switch-workspace-8)
                  ("s-9" . koek-wm/switch-workspace-9)
                  ("s-w" . koek-wm/switch-previous-workspace)
                  ("<f11>" . exwm-layout-toggle-fullscreen)
                  ("s-q" . bury-buffer)
                  ("s-d" . kill-current-buffer)
                  ("s-x" . koek-xde/launch-app)
                  ("s-C-f" . koek-xde/launch-file-manager)
                  ("s-C-b" . koek-xde/launch-browser)
                  ("s-s" . exwm-input-toggle-keyboard))))

  ;; Translate Emacs to non Emacs keybindings in line mode
  (setq exwm-input-simulation-keys
        (mapcar (pcase-lambda (`(,from . ,to))
                  (cons (kbd from) (kbd to)))
                koek-wm/base-simulation-keys)))

(use-package exwm-workspace
  :defer t
  :preface
  (defun koek-wm/classp (class &optional buffer)
    ;; `exwm-class-name' is the name of the application while
    ;; `exwm-instance-name' is the name of the instance of the
    ;; application, see
    ;; https://www.x.org/releases/X11R7.6/doc/xorg-docs/specs/ICCCM/icccm.html#wm_class_property.
    (when (boundp 'exwm-class-name)
      (when-let ((cl
                  (buffer-local-value
                   'exwm-class-name (get-buffer (or buffer (current-buffer))))))
        (let ((case-fold-search t))     ; Dynamic variable
          (string-match-p (regexp-quote class) cl)))))

  (defun koek-wm/get-firefox-page ()
    (string-match
     ;; Mozilla Firefox
     ;; Mozilla Firefox Private Browsing
     ;; Mozilla Firefox (Private Browsing)
     ;; URL - Title — Mozilla Firefox
     ;; URL - — Mozilla Firefox
     ;; URL - Title - Mozilla Firefox
     ;; URL - - Mozilla Firefox
     ;; URL - Title — Mozilla Firefox Private Browsing
     ;; URL - — Mozilla Firefox Private Browsing
     ;; URL - Title — Mozilla Firefox (Private Browsing)
     ;; URL - — Mozilla Firefox (Private Browsing)
     ;; URL - Title - Mozilla Firefox (Private Browsing)
     ;; URL - - Mozilla Firefox (Private Browsing)
     (rx line-start
         ;; URL
         (group-n 1 alpha (zero-or-more (any alnum "+-.")) ":" (minimal-match (zero-or-more not-newline)))
         ;; Separator
         " - "
         ;; Title
         (zero-or-one (group-n 2 (minimal-match (one-or-more not-newline))))
         ;; Suffix
         (zero-or-one " ") (any "-\N{EM DASH}") " Mozilla Firefox" (zero-or-one " " (or "Private Browsing" "(Private Browsing)"))
         line-end)
     exwm-title)
    (list :url (match-string 1 exwm-title) :title (match-string 2 exwm-title)))

  (defun koek-wm/update-current ()
    (cond
     ((koek-wm/classp "gimp")
      (exwm-workspace-rename-buffer "*GIMP*"))
     ((koek-wm/classp "firefox")
      (let* ((page (koek-wm/get-firefox-page))
             (title (plist-get page :title))
             (url (plist-get page :url))
             (parsed (url-generic-parse-url url))
             (scheme (url-type parsed))
             (id (if (member scheme '("about" "chrome"))
                     title
                   (or title url))))
        (exwm-workspace-rename-buffer
         (concat "*FF"
                 (when id
                   (concat ": " id))
                 "*"))
        ;; ibuffer, marginalia
        (setq list-buffers-directory url)))
     ((koek-wm/classp "microsoft teams")
      (exwm-workspace-rename-buffer "*Teams*"))
     ((koek-wm/classp "vlc")
      (exwm-workspace-rename-buffer "*VLC*"))
     (t
      (exwm-workspace-rename-buffer (format "*%s*" exwm-class-name)))))

  (defvar koek-wm/previous-workspace-n nil
    "Previously selected workspace number.")

  (defface koek-wm/selected-workspace '((t :inherit mode-line-emphasis))
    "Face for selected workspace label in mode line."
    :group 'exwm-workspace)

  (defface koek-wm/unselected-workspace nil
    "Face for unselected workspace label in mode line."
    :group 'exwm-workspace)

  (define-advice exwm-workspace-switch
      (:before (index &optional _force) koek-wm/update-previous-workspace-n)
    (unless (eq index exwm-workspace-current-index)
      (setq koek-wm/previous-workspace-n exwm-workspace-current-index)))

  (dolist (n (number-sequence 0 9))
    (defalias (intern (format "koek-wm/switch-workspace-%d" n))
      (lambda ()
        (interactive)
        (exwm-workspace-switch-create n))
      (format "Switch to workspace %d." n)))

  (defun koek-wm/switch-previous-workspace ()
    "Switch to previously selected workspace."
    (interactive)
    (when koek-wm/previous-workspace-n
      (exwm-workspace-switch-create koek-wm/previous-workspace-n)))

  (defun koek-wm/n-to-label (n)
    "Convert workspace number N to a workspace label.
N is an integer, a workspace number."
    (or (koek-ml/arabic-to-roman n) "N"))
  :config
  ;; Only when package is loaded
  (bind-keys
   ("C-c w 0" . koek-wm/switch-workspace-0)
   ("C-c w 1" . koek-wm/switch-workspace-1)
   ("C-c w 2" . koek-wm/switch-workspace-2)
   ("C-c w 3" . koek-wm/switch-workspace-3)
   ("C-c w 4" . koek-wm/switch-workspace-4)
   ("C-c w 5" . koek-wm/switch-workspace-5)
   ("C-c w 6" . koek-wm/switch-workspace-6)
   ("C-c w 7" . koek-wm/switch-workspace-7)
   ("C-c w 8" . koek-wm/switch-workspace-8)
   ("C-c w 9" . koek-wm/switch-workspace-9)
   ("C-c w w" . koek-wm/switch-previous-workspace)
   ("C-c w e" . exwm-workspace-swap)
   ("C-c w k" . exwm-workspace-delete))

  (setq exwm-workspace-number 2)
  (setq exwm-workspace-show-all-buffers t)
  (setq exwm-workspace-index-map #'koek-wm/n-to-label)
  (add-hook 'exwm-update-title-hook #'koek-wm/update-current))

(use-package exwm-layout
  :defer t
  :config
  (setq exwm-layout-show-all-buffers t))

(use-package exwm-manage
  :defer t
  :config
  (let ((defaults '(floating-mode-line nil)))
    (setq exwm-manage-configurations
          `(((koek-wm/classp "gimp")
             char-mode t ,@defaults)
            ((koek-wm/classp "inkscape")
             char-mode t ,@defaults)
            ((koek-wm/classp "firefox")
             simulation-keys
             ,(mapcar (pcase-lambda (`(,from . ,to))
                        (cons (kbd from) (kbd to)))
                      (append '(("M-o" . "C-n")
                                ("M-p" . "S-C-p")
                                ("M-k" . "C-w"))
                              koek-wm/base-simulation-keys))
             ,@defautls)))))

(use-package server
  :config
  (server-start))

(setq save-interprogram-paste-before-kill t)

(bind-key "C-z" #'repeat)

(defvar koek-xde/entries-dirs
  (mapcar (apply-partially #'expand-file-name "applications/")
          (cons (xdg-data-home) (xdg-data-dirs)))
  "List of directories where to look for desktop entries.
Directories are sorted from highest to lowest priority, i.e.,
earlier directories shadow entries in later ones.")

(defun koek-xde/read-entries ()
  (let ((file-names
         (thread-last koek-xde/entries-dirs
           (seq-filter #'file-accessible-directory-p)
           (seq-mapcat
            (lambda (file-name)
              (directory-files file-name
                               'full (rx ".desktop" line-end) 'nosort))))))
    (seq-reduce (lambda (entries file-name)
                  (let ((id (file-name-base file-name)))
                    (unless (gethash id entries)
                      (puthash id (xdg-desktop-read-file file-name) entries)))
                  entries)
                file-names (make-hash-table :test #'equal))))

(defun koek-xde/enabledp (entry desktops)
  (let ((no-display (gethash "NoDisplay" entry "false"))
        (hidden (gethash "Hidden" entry "false"))
        (onlys (split-string (gethash "OnlyShowIn" entry "") ";" 'omit-nulls))
        (nots (split-string (gethash "NotShowIn" entry "") ";" 'omit-nulls)))
    (null (or (string-equal no-display "true")
              (string-equal hidden "true")
              (and onlys (null (seq-intersection desktops onlys)))
              (and nots (seq-intersection desktops nots))))))

(defun koek-xde/get-entries ()
  (let ((entries (koek-xde/read-entries))
        (desktops (split-string (getenv "XDG_CURRENT_DESKTOP") path-separator))
        (disabled nil))
    (maphash (lambda (id entry)
               (unless (koek-xde/enabledp entry desktops)
                 (push id disabled)))
             entries)
    (dolist (id disabled)
      (remhash id entries))
    entries))

(defun koek-xde/make-completion-table (candidates)
  (let ((annotate (lambda (candidate)
                    (when-let ((comment (thread-last candidates
                                          (gethash candidate)
                                          (gethash "Comment"))))
                      (concat " " comment)))))
    (lambda (input pred action)
      (pcase action
        ('metadata
         `(metadata . ((category . xdg-desktop-entry)
                       (annotation-function . ,annotate))))
        (_action
         (complete-with-action action candidates input pred))))))

(defvar koek-xde/entry-history nil
  "History of entry names read.")

(defun koek-xde/read-id (prompt)
  (let* ((entries (koek-xde/get-entries))
         (ids (let ((ids nil))
                (maphash (lambda (id entry)
                           (push (cons (gethash "Name" entry) id) ids))
                         entries)
                ids))
         (candidates
          (seq-reduce (pcase-lambda (candidates `(,name . ,id))
                        (puthash name (gethash id entries) candidates)
                        candidates)
                      ids (make-hash-table :test #'equal))))
    (thread-first
        (completing-read prompt (koek-xde/make-completion-table candidates)
                         nil t nil 'koek-xde/entry-history)
      (assoc ids)
      cdr)))

(defun koek-xde/launch (id &rest uris)
  ;; default-directory
  (apply #'call-process "gtk-launch" nil 0 nil id
         (mapcar (lambda (uri)
                   (if (koek-subr/urip uri)
                       uri
                     (expand-file-name uri)))
                 uris)))

(defvar koek-xde/file-manager-id "org.gnome.Nautilus")
(defvar koek-xde/browser-id "firefox")

(defun koek-xde/launch-app (id)
  (interactive (list (koek-xde/read-id "Launch: ")))
  (koek-xde/launch id))

(defun koek-xde/launch-file-manager ()
  "Launch file manager."
  (interactive)
  (koek-xde/launch koek-xde/file-manager-id))

(defun koek-xde/launch-browser ()
  "Launch browser."
  (interactive)
  (koek-xde/launch koek-xde/browser-id))

(bind-keys
 ("C-c x x" . koek-xde/launch-app)
 ("C-c x C-f" . koek-xde/launch-file-manager)
 ("C-c x C-b" . koek-xde/launch-browser))

(unbind-key "C-x C-z")

(bind-keys
 ("C-c v m" . make-frame-command)
 ("C-c v o" . other-frame)
 ("C-c v d" . delete-frame)
 ("C-c v C-d" . delete-other-frames))

(setq window-resize-pixelwise t)

(bind-keys
 ("C-c w h" . split-window-below)
 ("C-c w v" . split-window-right)
 ("C-c w b" . balance-windows)
 ("C-c w d" . delete-window)
 ("C-c w C-d" . delete-other-windows)
 ("C-c w M-d" . kill-buffer-and-window))

(bind-key "C-c w C-b" #'balance-windows-area)

(defvar koek-wind/n-hor-steps 32
  "Number of horizontal steps to resize a window from monitor width to zero.")

(defun koek-wind/resize (shrink vertical)
  "Resize selected window.
When SHRINK is truthy, shrink window, else, grow window.  When
VERTICAL is truthy, resize vertically, else, resize
horizontally."
  (let* ((width (nth 2 (frame-monitor-geometry)))
         (step (/ width koek-wind/n-hor-steps))
         (delta (if shrink (* step -1) step)))
    (window-resize nil delta (not vertical) nil 'pixelwise)))

(defun koek-wind/grow (&optional arg)
  "Grow selected window.
With `\\[universal-argument]' prefix argument ARG, grow
vertically, else, grow horizontally."
  (interactive "P")
  (koek-wind/resize nil arg))

(defun koek-wind/shrink (&optional arg)
  "Shrink selected window.
With `\\[universal-argument]' prefix argument ARG, shrink
vertically, else, shrink horizontally."
  (interactive "P")
  (koek-wind/resize 'shrink arg))

(bind-keys
 ("C-c w g" . koek-wind/grow)
 ("C-c w s" . koek-wind/shrink))

(use-package ace-window
  :straight t
  :bind
  ([remap other-window] . ace-window)
  :preface
  (define-advice ace-window-display-mode
      (:around (f &rest args) koek-ace/disable-setup-mode-line)
    (let ((format (default-value 'mode-line-format)))
      (apply f args)
      (setq-default mode-line-format format)
      (force-mode-line-update 'all)))
  :config
  (setq aw-scope 'frame)
  (setq aw-swap-invert t)
  (setq aw-keys '(?q ?s ?d ?f ?j ?k ?l ?m))
  (setq aw-dispatch-alist '((?o aw-flip-window)))
  (setq aw-leading-char-style 'path)
  (ace-window-display-mode)
  ;; Ace isn't a minor mode but it can be delighted [sic]
  :delight)

(use-package transpose-frame
  :straight t
  :bind
  ("C-c w t" . transpose-frame)
  :preface
  (defun koek-tf/flip (&optional arg)
    "Flip window layout.
With `\\[universal-argument]' prefix argument ARG, flip
vertically, else, flip horizontally."
    (interactive "P")
    (if arg
        (flip-frame)
      (flop-frame)))

  (defun koek-tf/rotate (&optional arg)
    "Rotate window layout.
With `\\[universal-argument]' prefix argument ARG, rotate
clockwise, else, rotate counterclockwise."
    (interactive "P")
    (if arg
        (rotate-frame-clockwise)
      (rotate-frame-anticlockwise)))
  :init
  (bind-keys
   ("C-c w f" . koek-tf/flip)
   ("C-c w c" . koek-tf/rotate)))

(use-package eyebrowse
  :straight t
  :unless (string-equal (getenv "XDG_CURRENT_DESKTOP") "EXWM")
  :bind
  (("C-c w 0" . eyebrowse-switch-to-window-config-0)
   ("C-c w 1" . eyebrowse-switch-to-window-config-1)
   ("C-c w 2" . eyebrowse-switch-to-window-config-2)
   ("C-c w 3" . eyebrowse-switch-to-window-config-3)
   ("C-c w 4" . eyebrowse-switch-to-window-config-4)
   ("C-c w 5" . eyebrowse-switch-to-window-config-5)
   ("C-c w 6" . eyebrowse-switch-to-window-config-6)
   ("C-c w 7" . eyebrowse-switch-to-window-config-7)
   ("C-c w 8" . eyebrowse-switch-to-window-config-8)
   ("C-c w 9" . eyebrowse-switch-to-window-config-9)
   ("C-c w w" . eyebrowse-last-window-config)
   ("C-c w k" . eyebrowse-close-window-config))
  :init
  (setq eyebrowse-keymap-prefix (kbd "C-c w"))
  :config
  ;; Resolve keybinding conflict with transpose-frame
  (unbind-key "C-c w c" eyebrowse-mode-map)

  (setq eyebrowse-default-workspace-slot 0)
  (setq eyebrowse-mode-line-style 'hide)
  (eyebrowse-mode))

(use-package winner
  :bind
  (("C-c w l" . winner-undo)
   ("C-c w r" . winner-redo))
  :init
  (winner-mode))

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-trailing-separator-p t))

(defmacro koek-buff/def-category-pred (name)
  "Define buffer category predicate NAME."
  (let* ((prefix (replace-regexp-in-string
                  (rx (zero-or-one "-") "p" line-end) "" (symbol-name name)))
         (category (car (last (split-string prefix (rx (any "-/"))))))
         (modes-sym (intern (concat prefix "-modes")))
         (names-sym (intern (concat prefix "-names")))
         (fs-sym (intern (concat prefix "-fs")))
         (mode-pred-sym (intern (concat prefix "-mode-p")))
         (name-pred-sym (intern (concat prefix "-name-p"))))
    `(progn
       (defvar ,modes-sym nil
         ,(format "List of %s major mode symbols." category))

       (defvar ,names-sym nil
         ,(format "Regular expression matching names of %s buffers." category))

       (defvar ,fs-sym '(,mode-pred-sym ,name-pred-sym)
         ,(format "List of %s interrogation functions." category))

       (defun ,mode-pred-sym (&optional buffer)
         ,(string-join
           (list
            (format
             "Return whether major mode of BUFFER is derived from a %s mode."
             category)
            "Optional BUFFER is a buffer or string, respectively the buffer or"
            "name of the buffer to interrogate and defaults to the current"
            (format "buffer.  For %s major modes, see `%s'."
                    category modes-sym))
           "\n")
         (apply #'provided-mode-derived-p
                (buffer-local-value
                 'major-mode (get-buffer (or buffer (current-buffer))))
                ,modes-sym))

       (defun ,name-pred-sym (&optional buffer)
         ,(string-join
           (list
            (format "Return whether name of BUFFER is a %s name." category)
            "Optional BUFFER is a buffer or string, respectively the buffer or"
            "name of the buffer to interrogate and defaults to the current"
            (format "buffer.  For %s names, see `%s'." category names-sym))
           "\n")
         (when ,names-sym
           (let ((buffer-name (if (stringp buffer)
                                  buffer
                                (buffer-name (or buffer (current-buffer))))))
             (string-match ,names-sym buffer-name))))

       (defun ,name (&optional buffer)
         ,(string-join
           (list
            (format "Return whether BUFFER is a %s buffer." category)
            "Optional BUFFER is a buffer or string, respectively the buffer or"
            "name of the buffer to interrogate and defaults to the current"
            (format "buffer.  For %s interrogation functions, see" category)
            (format "`%s'." fs-sym))
           "\n")
         (with-current-buffer (get-buffer (or buffer (current-buffer)))
           (seq-some #'funcall ,fs-sym))))))

(koek-buff/def-category-pred koek-buff/dirp)
(koek-buff/def-category-pred koek-buff/docp)
(koek-buff/def-category-pred koek-buff/shellp)
(koek-buff/def-category-pred koek-buff/webp)

(defun koek-buff/doc-page-p (&optional buffer)
  "Return whether BUFFER displays a documentation webpage.
Optional BUFFER is a buffer or string, respectively the buffer or
name of the buffer to interrogate and defaults to the current
buffer."
  (let* ((url
          (buffer-local-value
           'list-buffers-directory (get-buffer (or buffer (current-buffer)))))
         (parsed (url-generic-parse-url url))
         (host (url-host parsed))
         (path (url-filename parsed)))
    (cond
     ((string-suffix-p "devdocs.io" host))
     ((string-suffix-p "lispworks.com" host)
      (or (string-prefix-p "/documentation" path)
          (string-prefix-p "/reference" path))))))

;; Directory buffers
(setq koek-buff/dir-modes '(dired-mode))

;; Help and documentation buffers
(setq koek-buff/doc-modes
      '(help-mode helpful-mode Info-mode Man-mode apropos-mode
        cider-docview-mode sly-apropos-mode geiser-doc-mode))
(setq koek-buff/doc-names
      (rx line-start
          (or "*eldoc*"
              "*cider-clojuredocs*"
              "*sly-description*"
              ;; When new buffer, major mode is set after calling
              ;; `display-buffer'
              "*info*"
              "*Man"
              ;; When no matches, major mode is `fundamental-mode'
              "*Apropos*")))
(setq koek-buff/doc-fs
      '(koek-buff/doc-mode-p koek-buff/doc-name-p koek-buff/doc-page-p))

;; Shell buffers
(setq koek-buff/shell-modes
      '(vterm-mode eshell-mode cider-repl-mode sly-mrepl-mode erlang-shell-mode
        indium-repl-mode inferior-octave-mode inferior-python-mode
        geiser-repl-mode sql-interactive-mode))
(setq koek-buff/shell-names
      (rx line-start "*" (zero-or-one (one-or-more (not "*")) "-")
          ;; When new buffer, major mode is set after calling
          ;; `display-buffer'
          (or "vterm*" "eshell*")))

;; Web buffers
(setq koek-buff/web-modes '(eww-mode))
(setq koek-buff/web-fs
      `(koek-buff/web-mode-p
        koek-buff/web-name-p
        ,(apply-partially #'koek-wm/classp "firefox")))

(defun koek-buff/bury (&optional arg)
  "Bury current.
With `\\[universal-argument]' prefix argument ARG, kill current."
  (interactive "P")
  (if arg
      (kill-buffer)
    (bury-buffer)))

(bind-key [remap kill-buffer] #'koek-buff/bury)

(use-package ibuffer
  :bind
  ([remap list-buffers] . ibuffer)
  :config
  (setq ibuffer-movement-cycle nil)
  (setq ibuffer-jump-offer-only-visible-buffers t)
  (setq ibuffer-formats
        '((mark
           " " (name 40 40 :left :elide)
           " " read-only modified
           " " (size 8 8 :right :elide)
           " " (mode 16 16 :left :elide)
           " " filename-and-process)))
  (setq ibuffer-eliding-string truncate-string-ellipsis))

(use-package ibuf-ext
  :after ibuffer
  :preface
  (defun koek-ibuf/urip (s)
    "Return whether S is a URI.
S is a string, the string to interrogate."
    (when s
      (string-match-p (rx line-start (one-or-more (any alnum "+-.")) ":") s)))

  (defun koek-ibuf/get-host (&optional buffer)
    (let ((url
           (buffer-local-value
            'list-buffers-directory (get-buffer (or buffer (current-buffer))))))
      (when (koek-ibuf/urip url)
        (let* ((parsed (url-generic-parse-url url))
               (host (url-host parsed)))
          (unless (string-empty-p host)
            host)))))

  (defun koek-ibuf/part-project-p (file-name)
    "Return whether current displays a file part of project FILE-NAME.
FILE-NAME is a string, the root of the project to compare with."
    (string-equal (koek-proj/locate-root) file-name))

  (defun koek-ibuf/part-host-p (host)
    "Return whether current displays a webpage part of HOST.
HOST is a string, the host to compare with."
    (string-equal (koek-ibuf/get-host) host))

  (defun koek-ibuf/group-project ()
    "Group buffers by project."
    (interactive)
    (let ((file-names (thread-last (buffer-list)
                        (mapcar #'koek-proj/locate-root)
                        (remq nil)
                        seq-uniq
                        (seq-sort #'string-lessp))))
      (setq ibuffer-filter-groups
            (mapcar (lambda (file-name)
                      `(,file-name
                        . ((predicate . (koek-ibuf/part-project-p ,file-name)))))
                    file-names))
      (ibuffer-update nil 'silent)))

  (defun koek-ibuf/group-host ()
    "Group buffers by host."
    (interactive)
    (let ((hosts (thread-last (buffer-list)
                   (mapcar #'koek-ibuf/get-host)
                   (remq nil)
                   seq-uniq
                   (seq-sort #'string-lessp))))
      (setq ibuffer-filter-groups
            (mapcar (lambda (host)
                      `(,host . ((predicate . (koek-ibuf/part-host-p ,host)))))
                    hosts))
      (ibuffer-update nil 'silent)))

  (defun koek-ibuf/clear-filters (&optional what)
    "Clear filters and filter groups.
WHAT is a symbol, the filter to clear, either filter (only
filters), group (only groups) or all (filters and groups)."
    (interactive
     (let ((what (pcase (prefix-numeric-value current-prefix-arg)
                   (4     'filter)
                   (16    'group)
                   (_what 'all))))
       (list what)))
    (let ((current (ibuffer-current-buffer)))
      (when (or (null what)
                (eq what 'filter)
                (eq what 'all))
        (setq ibuffer-filtering-qualifiers nil))
      (when (or (eq what 'group)
                (eq what 'all))
        (setq ibuffer-filter-groups nil))
      (ibuffer-update nil 'silent)
      (when current
        (ibuffer-jump-to-buffer (buffer-name current)))))
  :config
  (bind-keys
   :map ibuffer-mode-map
   ("\\ p" . koek-ibuf/group-project)
   ("\\ h" . koek-ibuf/group-host)
   ("/ /" . koek-ibuf/clear-filters))

  (setq ibuffer-saved-filters
        '(("Directory" . ((predicate . (koek-buff/dirp))))
          ("Help and documentation" . ((predicate . (koek-buff/docp))))
          ("Shell"     . ((predicate . (koek-buff/shellp))))
          ("Web"       . ((predicate . (koek-buff/webp))))
          ("Project"   . ((predicate . (koek-proj/locate-root))))))
  (setq ibuffer-show-empty-filter-groups nil))

(use-package minibuffer
  :defer t
  :config
  (use-package consult
    :bind
    (:map minibuffer-local-map
     ("C-r" . consult-history)))

  (setq enable-recursive-minibuffers t)
  (setq completion-in-region-function #'consult-completion-in-region)

  (let ((default-styles '(orderless basic)))
    (setq completion-styles default-styles)
    ;; completion-category-defaults
    (setq completion-category-overrides
          `((buffer       . ((styles . ,default-styles)))
            (email        . ((styles . ,default-styles)))
            ;; tramp
            (file         . ((styles . (basic ,@(remq 'basic default-styles)))))
            (info-menu    . ((styles . ,default-styles)))
            (project-file . ((styles . ,default-styles)))
            (unicode-name . ((styles . ,default-styles))))))

  (setq echo-keystrokes 0.3)

  ;; prompt
  (plist-put minibuffer-prompt-properties 'cursor-intangible t)
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode))

(use-package crm
  :defer t
  :preface
  (defface koek-mbuf/crm-indicator '((t . (:box t :inherit minibuffer-prompt)))
    "Face for CRM indicator in minibuffer prompt."
    :group 'minibuffer)

  (define-advice completing-read-multiple
      (:filter-args (args) koek-mbuf/insert-crm-indicator)
    (cons (replace-regexp-in-string
           (rx (group-n 1 (zero-or-one ": ")) line-end)
           (concat " " (propertize "CRM" 'face 'koek-mbuf/crm-indicator) "\\1")
           (car args))
          (cdr args))))

(use-package orderless
  :straight t
  :after minibuffer
  :preface
  (defun koek-rdls/dispatch (component _component-n _n-components)
    "Dispatch orderless component.
COMPONENT is a string, the component to dispatch.  Four patterns
are recognized:

- term@= is style `orderless-literal'
- term@, is style `orderless-initialism'
- term@$ is style `orderless-regexp'
- term@! is style `orderless-without-literal'"
    (when (string-match (rx (group-n 2 (one-or-more not-newline))
                            "@" (group-n 1 (any "=,$!")) line-end)
                        component)
      (let* ((dispatcher (match-string 1 component))
             (normalized (match-string 2 component))
             (style (pcase dispatcher
                      ("=" 'orderless-literal)
                      ("," 'orderless-initialism)
                      ("$" 'orderless-regexp)
                      ("!" 'orderless-without-literal))))
        (cons style normalized))))
  :config
  (setq orderless-component-separator #'orderless-escapable-split-on-space)
  (setq orderless-matching-styles
        '(orderless-literal orderless-initialism orderless-regexp))
  (setq orderless-style-dispatchers '(koek-rdls/dispatch)))

(use-package savehist
  :config
  (setq savehist-autosave-interval nil)
  (savehist-mode))

(use-package consult
  :straight t
  :bind
  (([remap switch-to-buffer] . consult-buffer)
   ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
   ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame)
   ("C-c f f" . consult-find)
   ([remap bookmark-jump] . consult-bookmark)
   ("C-§" . consult-line)
   ("M-s s" . consult-ripgrep)
   ("C-c j d" . consult-imenu)
   ("C-c e f" . consult-keep-lines)
   ([remap yank-pop] . consult-yank-pop))
  :preface
  (autoload #'bookmark-get-bookmark "bookmark")
  (autoload #'bookmark-get-handler "bookmark")

  (defvar koek-cslt/inhibited-buffer-modes '(exwm-mode))

  (defvar koek-cslt/inhibited-file-names
    (rx (or ".pdf" ".png" ".jpg") line-end))

  (defvar koek-cslt/inhibited-bookmark-handlers nil)

  (defun koek-cslt/inhibit-preview-p (candidate)
    (let* ((buffer (get-buffer candidate))
           (bookmark (unless buffer
                       (bookmark-get-bookmark candidate 'no-error)))
           (file-name (unless (or buffer bookmark)
                        candidate)))
      (cond
       (buffer
        (apply #'provided-mode-derived-p
               (buffer-local-value 'major-mode buffer)
               koek-cslt/inhibited-buffer-modes))
       (file-name
        (when koek-cslt/inhibited-file-names
          (string-match koek-cslt/inhibited-file-names file-name)))
       (bookmark
        (memq (or (bookmark-get-handler bookmark) 'bookmark-default-handler)
              koek-cslt/inhibited-bookmark-handlers)))))

  (defmacro koek-cslt/install-inhibit-preview (state-ctor candidate &rest body)
    (declare (indent 2))
    (let ((handler-sym (gensym))
          (action-sym (gensym))
          (candidate-sym (gensym)))
      `(define-advice ,state-ctor
           (:filter-return (,handler-sym) koek-cslt/inhibit-preview)
         (lambda (,action-sym ,candidate-sym)
           (if (and (eq ,action-sym 'preview)
                    ,candidate-sym
                    (koek-cslt/inhibit-preview-p ,candidate-sym))
               (progn
                 (let ((inhibit-message t)) ; Dynamic variable
                   (funcall ,handler-sym ,action-sym nil))
                 (let ((,candidate ,candidate-sym))
                   ,@body))
             (funcall ,handler-sym ,action-sym ,candidate-sym))))))

  (koek-cslt/install-inhibit-preview consult--buffer-state buffer-name
    (message "No preview for `%s'" buffer-name))

  (koek-cslt/install-inhibit-preview consult--file-state file-name
    (message "No preview for `%s'" (file-name-nondirectory file-name)))

  (koek-cslt/install-inhibit-preview consult--bookmark-state bookmark-name
    (message "No preview for `%s'" bookmark-name))

  (defvar koek-cslt/exwm-buffer-source
    `(:category buffer
      :items ,(lambda ()
                (thread-last
                  (buffer-list)
                  (seq-filter
                   (lambda (buffer)
                     (provided-mode-derived-p
                      (buffer-local-value 'major-mode buffer) 'exwm-mode)))
                  (mapcar #'buffer-name)))
      :history buffer-name-history
      :hidden t
      :narrow ?x
      :state consult--buffer-state
      :name "EXWM"
      :face consult-buffer))

  (defvar koek-cslt/dir-buffer-source
    `(:category buffer
      :items ,(lambda ()
                (thread-last (buffer-list)
                  (seq-filter #'koek-buff/dirp)
                  (mapcar #'buffer-name)))
      :history buffer-name-history
      :hidden t
      :narrow ?r
      :state consult--buffer-state
      :name "Directory"
      :face consult-buffer))

  (defvar koek-cslt/doc-buffer-source
    `(:category buffer
      :items ,(lambda ()
                (thread-last (buffer-list)
                  (seq-filter #'koek-buff/docp)
                  (mapcar #'buffer-name)))
      :history buffer-name-history
      :hidden t
      :narrow ?d
      :state consult--buffer-state
      :name "Help and documentation"
      :face consult-buffer))

  (defvar koek-cslt/shell-buffer-source
    `(:category buffer
      :items ,(lambda ()
                (thread-last (buffer-list)
                  (seq-filter #'koek-buff/shellp)
                  (mapcar #'buffer-name)))
      :history buffer-name-history
      :hidden t
      :narrow ?s
      :state consult--buffer-state
      :name "Shell"
      :face consult-buffer))

  (defvar koek-cslt/web-buffer-source
    `(:category buffer
      :items ,(lambda ()
                (thread-last (buffer-list)
                  (seq-filter #'koek-buff/webp)
                  (mapcar #'buffer-name)))
      :history buffer-name-history
      :hidden t
      :narrow ?w
      :state consult--buffer-state
      :name "Web"
      :face consult-buffer))
  :config
  (setq consult-narrow-key "C-+")
  (setq consult-buffer-sources
        '(consult--source-buffer
          consult--source-recent-file
          consult--source-bookmark
          consult--source-hidden-buffer
          koek-cslt/exwm-buffer-source
          koek-cslt/dir-buffer-source
          koek-cslt/doc-buffer-source
          koek-cslt/shell-buffer-source
          koek-cslt/web-buffer-source
          consult--source-project-buffer
          consult--source-project-recent-file))
  (setq consult-bookmark-narrow '((?f "File" bookmark-default-handler))))

(use-package embark
  :straight t
  :bind
  ("C-&" . embark-act)
  :config
  (use-package helpful
    :bind
    (:map embark-symbol-map
     ("h" . helpful-symbol)
     :map embark-become-help-map
     ("s" . helpful-symbol)
     ("v" . helpful-variable)
     ("f" . helpful-function)))

  (setq embark-help-key (kbd "?")))

(straight-use-package 'embark-consult)

(use-package vertico
  :straight (vertico :files (:defaults "extensions/*.el"))
  :config
  (use-package vertico-quick
    :bind
    (:map vertico-map
     ("C-c j" . vertico-quick-exit)))

  (setq vertico-resize t)
  (setq vertico-multiline
        (cons (propertize "\N{DOWNWARDS ARROW WITH TIP LEFTWARDS}" 'face 'vertico-multiline)
              (propertize truncate-string-ellipsis 'face 'vertico-multiline)))
  (vertico-mode))

(use-package vertico-repeat
  :bind
  ("M-z" . vertico-repeat)
  :hook (minibuffer-setup . vertico-repeat-save))

(use-package vertico-quick
  :defer t
  :config
  ;; Mirror avy
  (setq vertico-quick1 "qsdfjkl")
  (setq vertico-quick2 "m"))

(use-package marginalia
  :straight t
  ;; embark benefits from marginalia as it tries to guess the
  ;; completion category of completion tables missing one, see
  ;; `marginalia--completion-metadata-get'
  :after (:any embark vertico)
  :preface
  (defun koek-mgnl/builtin-annotator (candidate)
    "Annotator for types with a builtin annotator.
CANDIDATE is a string, the candidate to annotate.  Identical to
the builtin annotator except it aligns the annotation."
    (when-let* ((annotate (alist-get 'annotation-function marginalia--metadata))
                (annotation (funcall annotate candidate)))
      (marginalia--fields
       ((string-trim annotation)
        :truncate 1.0 :face 'completions-annotations))))
  :config
  (push '(xdg-desktop-entry . (koek-mgnl/builtin-annotator builtin none))
        marginalia-annotator-registry)
  (marginalia-mode))

(use-package dired
  :hook (dired-mode . dired-hide-details-mode)
  :config
  (autoload #'dired-dwim-target-next-visible "dired-aux")

  (setq dired-dwim-target #'dired-dwim-target-next-visible)
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (let* ((safe "-lah") ; For safe switches, see `ls-lisp--insert-directory'
         (unsafe (concat safe " --group-directories-first")))
    (setq dired-listing-switches (if (executable-find "ls") unsafe safe))))

(use-package dired-aux
  :after dired
  :config
  (setq dired-vc-rename-file t)
  (setq dired-create-destination-dirs 'ask))

(use-package dired-x
  :after dired
  :bind
  ("C-c f d" . dired-jump))

(use-package ls-lisp
  :defer t
  :config
  (setq ls-lisp-dirs-first t))

(use-package diredfl
  :straight t
  :after dired
  :config
  (diredfl-global-mode))

(use-package recentf
  :config
  (require 'find-func)

  (setq recentf-max-saved-items 100)

  ;; Ignore Emacs libraries
  (let ((file-names
         (list
          (rx line-start
              (literal (locate-dominating-file (find-library-name "files")
                                               emacs-version)))
          ;; True and symbolic file name variants
          (regexp-quote (file-relative-name user-emacs-directory "~/")))))
    (setq recentf-exclude (append file-names recentf-exclude)))

  (recentf-mode))

(use-package saveplace
  :config
  (save-place-mode))

(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

(use-package autorevert
  :config
  (global-auto-revert-mode)
  :delight auto-revert-mode)

(setq delete-by-moving-to-trash t)

(use-package project
  :straight t
  :defer t
  :preface
  (defvar-local koek-proj/cache nil
    "Project cache.")

  (defun koek-proj/locate-root (&optional buffer)
    "Locate root of project for BUFFER.
Optional BUFFER is a buffer or string, respectively the buffer or
name of the buffer to locate the root for and defaults to the
current buffer.

This function is expensive.  For a potentially cheap alternative,
see `koek-proj/get-root'."
    (let ((file-name
           (buffer-local-value
            'default-directory (get-buffer (or buffer (current-buffer))))))
      (when-let ((project (project-current nil file-name)))
        (project-root project))))

  (defun koek-proj/determine-name (file-name)
    "Determine name of project for project FILE-NAME."
    (file-name-nondirectory (directory-file-name file-name)))

  (defun koek-proj/init ()
    "Initialize project cache of current."
    (unless koek-proj/cache
      (setq koek-proj/cache
            (if-let ((file-name (koek-proj/locate-root)))
                (list :root file-name
                      :name (koek-proj/determine-name file-name))
              'no-project))))

  (defun koek-proj/get-root ()
    "Return cached root of project for current.
The first call in a buffer is expensive.  Subsequent calls in the
same buffer are cheap but its results incorrect when the buffer
is no longer part of the initial project.  For a correct
alternative, see `koek-proj/locate-root'."
    (koek-proj/init)
    (plist-get koek-proj/cache :root))

  (defun koek-proj/get-name ()
    "Return cached name of project for current.
The first call in a buffer is expensive.  Subsequent calls in the
same buffer are cheap but its results incorrect when the buffer
is no longer part of the initial project."
    (koek-proj/init)
    (plist-get koek-proj/cache :name))

  (defun koek-proj/locate-configs (name &optional buffer prompt)
    "Locate configuration files of project for BUFFER.
Some projects contain configuration files (e.g. Makefile,
project.clj and package.json), one in the root directory, the
primary configuration file, and/or one or more in the child
directories of the root directory, the secondary configuration
files.

NAME is a string, the name of the configuration file.  Optional
BUFFER is a buffer or string, respectively the buffer or name of
the buffer to locate the configuration files for and defaults to
the current buffer.

When optional PROMPT is truthy and the project only contains
secondary configuration files, prompt for one.  The selected
configuration file is sorted before the other ones.

When the project contains both a primary configuration file and
secondary configuration files, the primary one is sorted before
the secondary ones."
    (let* ((root (or (koek-proj/locate-root buffer) (error "Not in a project")))
           (primary (let ((file-name (expand-file-name name root)))
                      (when (file-exists-p file-name)
                        (list file-name))))
           (secondary (file-expand-wildcards (thread-last root
                                               (expand-file-name "*/")
                                               (expand-file-name name)))))
      (if (and (null primary) (> (length secondary) 1) prompt)
          (let* ((candidates (mapcar (lambda (file-name)
                                       (cons (file-relative-name file-name root)
                                             file-name))
                                     secondary))
                 (file-name (thread-first
                                (completing-read "Project configuration: "
                                                 candidates nil t)
                              (assoc candidates)
                              cdr)))
            (cons file-name (remove file-name secondary)))
        (append primary secondary))))

  (define-advice project-remember-project
      (:before-while (project) koek-proj/exclude-uninteresting)
    (if (bound-and-true-p recentf-mode)
        (recentf-include-p (project-root project))
      t))

  (defun koek-proj/magit-status ()
    "Launch magit in current project.
When not in a project, prompt for one."
    (interactive)
    (let ((default-directory            ; Dynamic variable
            (project-root (project-current 'maybe-prompt))))
      (magit-status-setup-buffer)))

  (defun koek-proj/consult-ripgrep ()
    "Launch ripgrep in current project.
When not in a project, prompt for one."
    (interactive)
    (let ((default-directory            ; Dynamic variable
            (project-root (project-current 'maybe-prompt))))
      (consult-ripgrep)))

  (defun koek-proj/vterm (&optional arg)
    "Launch or switch to a vterm session in current project.
With numeric prefix argument ARG, launch or switch to a numbered
vterm session.  With `\\[universal-argument]' prefix argument
ARG, launch a new vterm session.  When not in a project, prompt
for one."
    (interactive "P")
    ;; Dynamic variables
    (defvar vterm-buffer-name)
    (let* ((default-directory (project-root (project-current 'maybe-prompt)))
           (vterm-buffer-name (project-prefixed-buffer-name "vterm")))
      (vterm arg)))

  ;; `project-eshell' doesn't launch or switch to numbered eshell
  ;; sessions
  (defun koek-proj/eshell (&optional arg)
    "Launch or switch to an eshell session in current project.
With numeric prefix argument ARG, launch or switch to a numbered
eshell session.  With `\\[universal-argument]' prefix argument
ARG, launch a new eshell session.  When not in a project, prompt
for one."
    (interactive "P")
    ;; Dynamic variables
    (defvar eshell-buffer-name)
    (let* ((default-directory (project-root (project-current 'maybe-prompt)))
           (eshell-buffer-name (project-prefixed-buffer-name "eshell")))
      (eshell arg)))
  :config
  (use-package consult
    :bind
    (:map project-prefix-map
     ("b" . consult-project-buffer)))

  (bind-keys
   :map project-prefix-map
   ("m" . koek-proj/magit-status)
   ("s" . koek-proj/consult-ripgrep)
   ("t" . koek-proj/vterm)
   ("e" . koek-proj/eshell))

  (setq project-compilation-buffer-name-function #'project-prefixed-buffer-name)
  (setq project-switch-commands
        '((project-find-file "Find file")
          (project-dired "Dired")
          (project-find-dir "Find directory")
          (consult-project-buffer "Buffer")
          (koek-proj/magit-status "Magit")
          (koek-proj/consult-ripgrep "Ripgrep")
          (koek-proj/vterm "Vterm")
          (koek-proj/eshell "Eshell"))))

(use-package find-func
  :bind
  ("C-c f l" . find-library))

(use-package vc-hooks
  :defer t
  :config
  (setq vc-follow-symlinks t))

(use-package diff-hl
  :straight t
  :bind
  ("C-c a c" . diff-hl-mode)            ; [C]hanges
  :config
  (setq diff-hl-draw-borders nil))

(use-package magit-mode
  :straight magit
  :defer t
  :config
  (setq magit-bury-buffer-function #'magit-mode-quit-window))

(use-package magit-status
  :bind
  ("C-c f m" . magit-status))

(use-package magit-diff
  :defer t
  :config
  (setq magit-diff-refine-hunk t))

(use-package magit-blame
  :defer t
  :config
  (setq magit-blame-styles
        '((margin (margin-format    . ("%s%f" "%C %a" "%H" ""))
                  (margin-width     . 40)
                  (margin-face      . magit-blame-margin)
                  (margin-body-face . (magit-blame-dimmed)))
          (heading (heading-format . "%C %-20a %s\n"))
          (line (show-message . t)
                (show-lines   . t))))
  (setq magit-blame-echo-style 'line))

(use-package git-commit
  :defer t
  :preface
  (defun koek-git/check-spelling (force)
    "Check spelling of commit message.
When FORCE is truthy, continue commit unconditionally."
    (let ((tick (buffer-chars-modified-tick))
          (result
           (let ((ispell-skip-region-alist ; Dynamic variable
                  (cons (list (rx line-start "#") #'forward-line) ; Comment
                        ispell-skip-region-alist)))
             (ispell-buffer))))
      (cond
       (force
        t)
       ;; When spell check was completed, result is truthy
       (result
        ;; When nothing was corrected, character tick counter is
        ;; unchanged
        (or (= (buffer-chars-modified-tick) tick)
            (y-or-n-p "Spelling checked.  Commit? "))))))
  :config
  (add-hook 'git-commit-finish-query-functions #'koek-git/check-spelling))

(use-package ediff
  :bind
  (("C-c f e" . ediff-files)
   ("C-c f C-e" . ediff-current-file))
  :preface
  (defface koek-diff/variant '((t :inherit mode-line-emphasis))
    "Face for variant label in mode line."
    :group 'ediff))

(use-package ediff-init
  :defer t
  :preface
  (defun koek-diff/unfold-outline ()
    "Unfold outline in outline and derived modes."
    (when (derived-mode-p 'outline-mode) ; org-mode derives from outline-mode
      (outline-show-all)))
  :config
  (add-hook 'ediff-prepare-buffer-hook #'koek-diff/unfold-outline))

(use-package ediff-util
  :defer t
  :preface
  ;; `ediff-before-setup-hook' is too early, the control buffer is not
  ;; yet created. `ediff-startup-hook' is too late, the window
  ;; configuration was already modified.
  (define-advice ediff-setup
      (:around (f &rest args) koek-diff/setup-restore-window-config)
    (let ((config (current-window-configuration))
          (control-buffer (apply f args)))
      (with-current-buffer control-buffer
        (add-hook 'ediff-quit-hook
                  (lambda ()
                    (set-window-configuration config))
                  'append 'local))
      control-buffer)))

(use-package ediff-wind
  :defer t
  :config
  (setq ediff-grab-mouse nil)
  (setq ediff-window-setup-function #'ediff-setup-windows-plain)
  (setq ediff-split-window-function #'split-window-right))

(use-package wgrep
  :straight t
  :after grep
  :config
  (setq wgrep-enable-key (kbd "C-x C-q")))

(use-package avy
  :straight t
  :bind
  (("C-c j j" . avy-goto-char-timer)
   ([remap goto-line] . avy-goto-line))
  :config
  (setq avy-all-windows nil)
  (setq avy-all-windows-alt 'all-frames)
  (setq avy-keys '(?q ?s ?d ?f ?j ?k ?l ?m))
  (setq avy-dispatch-alist nil)
  ;; Hints can touch, making it difficult to identify individual ones.
  ;; Style all hint characters identical except the first one.
  (setq avy-lead-faces
        '(avy-lead-face
          avy-lead-face-1
          avy-lead-face-1
          avy-lead-face-1
          avy-lead-face-1)))

(use-package link-hint
  :straight t
  :bind
  (("C-c j l" . link-hint-open-link)
   ("C-c j C-l" . link-hint-copy-link))
  :preface
  (defun koek-lh/next-dictionary-link (limit)
    "Return position of next dictionary link.
LIMIT is a position, a search limit limiting dictionary links to
dictionary links before LIMIT."
    (link-hint--next-property 'link limit))

  (defun koek-lh/point-at-dictionary-link-p ()
    "Return whether point is at a dictionary link."
    (get-text-property (point) 'link))

  (defun koek-lh/dictionary-mode-p ()
    "Return whether current major mode is derived from dictionary-mode."
    (derived-mode-p 'dictionary-mode))

  (defun koek-lh/open-dictionary-link ()
    "Open dictionary link at point."
    (link-selected))
  :config
  (link-hint-define-type 'dictionary-link
    :next #'koek-lh/next-dictionary-link
    :at-point-p #'koek-lh/point-at-dictionary-link-p
    ;; dictionary-mode doesn't bind the symbol dictionary-mode
    :predicates (list #'koek-lh/dictionary-mode-p)
    :open #'koek-lh/open-dictionary-link)
  (push 'link-hint-dictionary-link link-hint-types))

(define-advice pop-to-mark-command (:around (f) koek-mark/ensure-move)
  (let ((start (point))
        (n (length mark-ring)))
    ;; Move point to current mark
    (funcall f)
    ;; Move point to previous marks in mark ring
    (while (and (= (point) start) (> n 0))
      (funcall f)
      (setq n (1- n)))))

(put 'narrow-to-region 'disabled nil)

(use-package sort
  :bind
  (("C-c e s" . sort-lines)
   ("C-c e k" . delete-duplicate-lines)))

(use-package align
  :bind
  ("C-c e a" . align-regexp))

(use-package undo-tree
  :straight t
  :bind
  (:map undo-tree-map
   ("M-/" . undo-tree-redo))
  :init
  (global-undo-tree-mode)
  :delight)

(use-package ispell
  :bind
  ("C-c e d" . ispell-change-dictionary)
  :config
  (setq ispell-program-name "hunspell")
  (let ((dictionary-name "en_US"))
    ;; On Windows, Hunspell expects the DICTIONARY environment
    ;; variable to be set
    (when (eq system-type 'windows-nt)
      (setenv "DICTIONARY" dictionary-name))
    (setq ispell-dictionary dictionary-name)))

(use-package display-line-numbers
  :bind ("C-c a n" . display-line-numbers-mode)) ; Line [n]umbers

(use-package olivetti
  :straight t
  :bind
  ("C-c a m" . olivetti-mode)           ; [M]argins
  :config
  (setq olivetti-body-width (round (* 80 1.25)))
  :delight)

(use-package display-fill-column-indicator
  :bind
  ("C-c a r" . display-fill-column-indicator-mode) ; [R]ight margin
  :preface
  (defvar koek-fi/column 80
    "Column in which to draw the indicator.")

  (defun koek-fi/setup-indicator ()
    "Setup indicator in current."
    (setq display-fill-column-indicator-column koek-fi/column))
  :init
  (add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)
  (add-hook 'prog-mode-hook #'koek-fi/setup-indicator)
  (add-hook 'conf-mode-hook #'display-fill-column-indicator-mode)
  (add-hook 'conf-mode-hook #'koek-fi/setup-indicator))

(use-package face-remap
  :hook ((markdown-mode org-mode) . variable-pitch-mode)
  :delight buffer-face-mode)

(use-package hl-line
  :bind ("C-c a l" . hl-line-mode)      ; [L]ine
  :hook
  ((ibuffer-mode embark-collect-mode dired-mode occur-mode grep-mode proced-mode
    bongo-playlist-mode)
   . hl-line-mode))

(use-package expand-region
  :straight t
  :bind
  ("C-S-SPC" . er/expand-region)
  :config
  (setq expand-region-smart-cursor t))

(bind-keys
 ([remap downcase-word]   . downcase-dwim)
 ([remap upcase-word]     . upcase-dwim)
 ([remap capitalize-word] . capitalize-dwim))

(use-package subword
  :hook
  ((prog-mode conf-mode eshell-mode comint-mode cider-repl-mode indium-repl-mode) .
   subword-mode)
  :delight)

(defun koek-mtn/next-word (&optional arg)
  "Move point to beginning of next word, repeat ARG times.
Optional ARG is an integer and defaults to one.  When ARG is
negative, move point to ending of previous word."
  (interactive "p")
  (unless arg
    (setq arg 1))
  (unless (= arg 0)
    (let ((step (/ arg (abs arg))))
      (when (or (and (> step 0) (looking-at (rx word)))
                (and (< step 0)
                     (looking-back (rx word) (max (1- (point)) (point-min)))))
        (forward-word step))
      (forward-word (- arg step))
      (when (forward-word step)
        (backward-word step)))))

(defun koek-mtn/previous-word (&optional arg)
  "Move point to ending of previous word, repeat ARG times.
Optional ARG is an integer and defaults to one.  When ARG is
negative, move point to beginning of next word."
  (interactive "p")
  (unless arg
    (setq arg 1))
  (koek-mtn/next-word (- arg)))

(bind-keys
 ("M-n" . koek-mtn/next-word)
 ("M-p" . koek-mtn/previous-word))

(delight 'visual-line-mode nil 'simple)

(add-hook 'koek-txt/confident-hook #'auto-fill-mode)
(delight 'auto-fill-function nil 'simple)

(use-package smartparens
  :straight t
  :bind
  (:map smartparens-mode-map
   ("C-M-f" . sp-forward-sexp)
   ("C-M-b" . sp-backward-sexp)
   ("C-M-n" . sp-next-sexp)
   ("C-M-p" . sp-previous-sexp)
   ("C-M-a" . sp-beginning-of-sexp)
   ("C-M-e" . sp-end-of-sexp)
   ("C-M-d" . sp-down-sexp)
   ("C-M-u" . sp-up-sexp)
   ("C-S-d" . sp-backward-down-sexp)
   ("C-S-u" . sp-backward-up-sexp)
   ("C-M-t" . sp-transpose-sexp)
   ("C-M-(" . sp-split-sexp)
   ("C-M-<right>" . sp-forward-slurp-sexp)
   ("C-M-<left>"  . sp-forward-barf-sexp)
   ("C-S-<left>"  . sp-backward-slurp-sexp)
   ("C-S-<right>" . sp-backward-barf-sexp)
   ("C-M-<down>"  . sp-unwrap-sexp)
   ("C-M-w" . sp-copy-sexp)
   ("C-M-k" . sp-kill-sexp))
  :hook
  (((prog-mode conf-mode eshell-mode comint-mode cider-repl-mode indium-repl-mode) .
    smartparens-mode)
   (smartparens-mode . show-smartparens-mode))
  :preface
  (defun koek-sp/separate-sexp (open-delimiter action _context)
    "Separate just inserted sexp from previous and/or next sexp.
OPEN-DELIMITER is a string, the delimiter inserted.  ACTION is a
symbol, the action performed, see `sp-pair'.  _CONTEXT is
ignored."
    (when (and (eq action 'insert)
               ;; Outer context, _context is inner context
               (save-excursion
                 (search-backward open-delimiter)
                 (eq (sp--get-context) 'code)))
      (save-excursion
        (search-backward open-delimiter)
        (unless (looking-back (rx (or (any "#'`,~@([{" blank) line-start))
                              (max (1- (point)) (point-min)))
          (insert " "))
        (search-forward open-delimiter)
        (search-forward (sp-get-pair open-delimiter :close))
        (unless (looking-at (rx (or (any ")]}" blank) line-end)))
          (insert " ")))))

  (defun koek-sp/setup-separate-sexp-handler (mode &rest open-delimiters)
    "Setup separate-sexp handler in MODE for OPEN-DELIMITERS.
MODE is a symbol, the mode to setup.  OPEN-DELIMITERS are one or
more strings, the delimiters that call the handler."
    (dolist (delimiter open-delimiters)
      (sp-local-pair mode delimiter nil
                     :post-handlers '(:add koek-sp/separate-sexp))))

  (defun koek-sp/format-c-block (open-delimiter action _context)
    "Format just inserted multiple line C block.
OPEN-DELIMITER is a string, the delimiter inserted.  ACTION is a
symbol, the action performed, see `sp-pair'.  _CONTEXT is
ignored."
    (when (and (eq action 'insert)
               (save-excursion
                 (search-backward open-delimiter)
                 (eq (sp--get-context) 'code)))
      (save-excursion
        (insert "\n")
        (indent-according-to-mode))
      (indent-according-to-mode)))

  (defun koek-sp/setup-format-c-block-on-return-handler
      (mode &rest open-delimiters)
    "Setup format-c-block handler in MODE for OPEN-DELIMITERS.
MODE is a symbol, the mode to setup.  OPEN-DELIMITERS are one or
more strings, the delimiters that call the handler."
    (dolist (delimiter open-delimiters)
      (sp-local-pair mode delimiter nil
                     ;; For event names, see `single-key-description'
                     :post-handlers '(:add (koek-sp/format-c-block "RET")))))
  :init
  (bind-key "C-S-w" #'append-next-kill)
  :config
  (require 'smartparens-config)

  (setq sp-navigate-interactive-always-progress-point t)
  (setq sp-navigate-reindent-after-up nil)
  (setq sp-highlight-pair-overlay nil)
  (koek-sp/setup-separate-sexp-handler 'clojure-mode "(" "[" "{" "\"")
  (koek-sp/setup-separate-sexp-handler 'cider-repl-mode "(" "[" "{" "\"")
  (koek-sp/setup-separate-sexp-handler 'lisp-mode "(" "\"")
  (koek-sp/setup-separate-sexp-handler 'emacs-lisp-mode "(" "[" "\"")
  (koek-sp/setup-separate-sexp-handler 'scheme-mode "(" "\"")
  (koek-sp/setup-format-c-block-on-return-handler 'c-mode "{")
  (koek-sp/setup-format-c-block-on-return-handler 'c++-mode "{")
  (koek-sp/setup-format-c-block-on-return-handler 'css-mode "{")
  (koek-sp/setup-format-c-block-on-return-handler 'java-mode "{")
  (koek-sp/setup-format-c-block-on-return-handler 'js-mode "{" "[")
  (koek-sp/setup-format-c-block-on-return-handler 'json-mode "{" "[")
  (koek-sp/setup-format-c-block-on-return-handler 'scad-mode "{")
  (koek-sp/setup-format-c-block-on-return-handler 'python-mode "{" "[" "(")
  :delight)

(use-package lisp
  :bind
  (("C-M-{" . beginning-of-defun)
   ("C-M-}" . end-of-defun)))

(use-package paren-face
  :straight t
  :hook
  ((clojure-mode cider-repl-mode lisp-mode sly-mrepl-mode
    emacs-lisp-mode lisp-interaction-mode scheme-mode geiser-repl-mode) .
   paren-face-mode)
  :config
  (setq paren-face-regexp (rx (any "()[]{}"))))

(setq-default indent-tabs-mode nil)

(setq sentence-end-double-space nil)

(setq require-final-newline t)

(bind-key "C-c e w" #'delete-trailing-whitespace)

(use-package tabify
  :bind
  (("C-c e SPC" . untabify)
   ("C-c e TAB" . tabify)))

(use-package ws-butler
  :straight t
  :hook ((prog-mode conf-mode) . ws-butler-mode)
  :delight)

(use-package whitespace
  :hook ((prog-mode conf-mode) . whitespace-mode)
  :config
  (setq whitespace-style '(tab-mark face tabs))
  (setq whitespace-display-mappings
        '((tab-mark ?\t [?\N{RIGHTWARDS ARROW TO BAR} ?\t])))
  :delight)

(use-package eglot
  :straight t
  :bind
  (:map eglot-mode-map
   ("C-c e x" . eglot-code-actions)
   ("C-c e r" . eglot-rename))
  :hook
  ((c-mode c++-mode erlang-mode java-mode js-mode python-mode) . eglot-ensure)
  :preface
  (defvar koek-eglt/jdtls-dirs
    (mapcar (apply-partially #'expand-file-name "java/jdtls/")
            (cons (xdg-data-home) (xdg-data-dirs)))
    "List of directories where to look for JDT LS.
Directories are sorted from highest to lowest priority, i.e.,
earlier directories shadow later ones.")

  (defun koek-eglt/init-clangd (root db)
    (interactive
     (let* ((root (or (koek-proj/locate-root) (user-error "Not in a project")))
            (default
              (let ((db (expand-file-name "build/compile_commands.json" root)))
                (when (file-exists-p db)
                  db)))
            (db
             (or default
                 (expand-file-name
                  (read-file-name "Compilation database: " root nil t) root))))
       (list root db)))
    (make-symbolic-link (file-relative-name (expand-file-name db root) root)
                        (expand-file-name "compile_commands.json" root)
                        'overwrite))
  :config
  ;; JDT LS lacks an executable. Eglot expects to find the JDT LS
  ;; launcher on the CLASSPATH environment variable.
  (when-let
      ((file-name
        (thread-last koek-eglt/jdtls-dirs
          (mapcar (apply-partially #'expand-file-name "plugins/"))
          (seq-filter #'file-exists-p)
          (seq-mapcat (lambda (file-name)
                        (directory-files file-name 'full
                                         (rx "org.eclipse.equinox.launcher_"
                                             (one-or-more (or alnum punct))
                                             ".jar" line-end))))
          car)))
    (let ((paths (parse-colon-path (getenv "CLASSPATH"))))
      (unless (member file-name paths)
        (setenv "CLASSPATH"
                (string-join (cons file-name paths) path-separator))))))

(use-package tree-sitter
  :straight t
  :defer t
  :delight)

(use-package tree-sitter-langs
  :straight t
  :after tree-sitter)

(use-package tree-sitter-hl
  :hook ((c-mode c++-mode java-mode js-mode) . tree-sitter-hl-mode))

(use-package xref
  :straight t
  :defer t
  :config
  (setq xref-show-xrefs-function #'consult-xref)
  (setq xref-show-definitions-function #'consult-xref)
  (add-to-list 'xref-prompt-for-identifier #'xref-find-references 'append))

(use-package flymake
  :straight t
  :bind
  (:map flymake-mode-map
   ("C-c e n" . flymake-goto-next-error)
   ("C-c e p" . flymake-goto-prev-error)
   ("C-c e l" . flymake-show-diagnostics-buffer))
  :hook ((clojure-mode emacs-lisp-mode) . flymake-mode)
  :config
  (setq flymake-wrap-around nil)
  :delight)

(use-package flymake-proc
  :defer t
  :config
  (remove-hook 'flymake-diagnostic-functions #'flymake-proc-legacy-flymake))

(use-package flymake-kondor
  :straight t
  :hook (clojure-mode . flymake-kondor-setup)
  :preface
  (defun koek-kndr/init (root &optional interactive)
    (interactive
     (let ((root (or (koek-proj/locate-root) (user-error "Not in a project"))))
       (list root 'interactive)))
    (when interactive
      (message "Initializing clj-kondo..."))
    (let* ((default-directory root)     ; Dynamic variable
           (build-system
            (cond
             ((file-exists-p "project.clj")
              'lein)
             ((file-exists-p "shadow-cljs.edn")
              'shadow)
             (t
              (error
               "No supported build system found, supported are Leiningen and shadow-cljs"))))
           (classpath
            (with-temp-buffer
              (let ((result
                     (pcase build-system
                       ('lein
                        (call-process "lein" nil '(t nil) nil "classpath"))
                       ('shadow
                        (call-process "npx" nil '(t nil) nil
                                      "shadow-cljs" "classpath")))))
                (unless (zerop result)
                  (error "%s returned %d"
                         (pcase build-system
                           ('lein "Leiningen")
                           ('shadow "shadow-cljs"))
                         result)))
              (buffer-substring (point-min) (point-max))))
           (result
            (progn
              (make-directory ".clj-kondo" 'no-error)
              (call-process "clj-kondo" nil nil nil
                            "--lint" classpath
                            "--parallel" "--dependencies" "--copy-configs"))))
      (unless (zerop result)
        (error "clj-kondo returned %d" result)))
    (when interactive
      (message "Initializing clj-kondo...done"))))

(setq tab-always-indent 'complete)

(use-package abbrev
  :hook ((sql-mode sql-interactive-mode) . abbrev-mode)
  :delight)

(use-package yasnippet
  :straight t
  :hook ((text-mode prog-mode conf-mode) . yas-minor-mode)
  :preface
  ;; General
  (defun koek-ys/indent-snippet ()
    "Indent last expanded snippet.
Snippet is between `yas-snippet-beg' and `yas-snippet-end'."
    (indent-region yas-snippet-beg yas-snippet-end))

  (defun koek-ys/complete-field (candidates)
    "Complete field from CANDIDATES.
CANDIDATES is an alist of pretty candidate to candidate pairs."
    (cdr (assoc (yas-choose-value (mapcar #'car candidates)) candidates)))

  ;; Clojure & ClojureScript
  (defun koek-ys/determine-ns-name ()
    "Determine Clojure namespace name for current.
Assumes source path is the root of the project."
    (let ((root (or (koek-proj/locate-root) default-directory))
          (file-name (or (buffer-file-name) (buffer-name)))
          (separator (thread-first (expand-file-name "a" "b")
                       file-relative-name
                       (substring 1 2))))
      (thread-last (file-relative-name file-name root)
        file-name-sans-extension
        (replace-regexp-in-string (regexp-quote separator) ".")
        (replace-regexp-in-string "_" "-"))))

  ;; Org & Markdown
  (defvar koek-ys/languages
    '((:ietf "de-DE" :org "de-de" :tex "ngerman"  :hun "de_DE")
      (:ietf "en-US" :org "en-us" :tex "american" :hun "en_US")
      (:ietf "fr-FR" :org "fr"    :tex "frenchb"  :hun "fr_FR")
      (:ietf "nl-BE" :org "nl"    :tex "dutch"    :hun "nl_BE"))
    "List of language specifications.
A language specification is a plist with keys :ietf, :org, :tex
and :hun.  :ietf is a string, an IETF language code.  :org, :tex
and :hun are strings, the Org, LaTeX and Hunspell language
code.")

  (defun koek-ys/lang-to-other (lang from to)
    "Translate language code LANG from scheme FROM to TO.
FROM is a symbol, the language scheme of code LANG.  TO is a
symbol, the language scheme to.  For language schemes, see
`koek-ys/languages'."
    (plist-get (seq-find (lambda (spec)
                           (string-equal (plist-get spec from) lang))
                         koek-ys/languages)
               to))

  (defun koek-ys/ietf-to-other (lang to)
    "Translate IETF language code LANG to scheme TO.
TO is a symbol, the language scheme to, see
`koek-ys/lang-to-other'."
    (koek-ys/lang-to-other lang :ietf to))

  (defun koek-ys/org-to-other (lang to)
    "Translate Org language code LANG to scheme TO.
TO is a symbol, the language scheme to, see
`koek-ys/lang-to-other'."
    (koek-ys/lang-to-other lang :org to))

  (defun koek-ys/complete-ietf ()
    "Complete IETF language code."
    (yas-choose-value (mapcar (lambda (spec)
                                (plist-get spec :ietf))
                              koek-ys/languages)))

  (defun koek-ys/complete-org ()
    "Complete Org language code from IETF codes."
    (koek-ys/complete-field (mapcar (lambda (spec)
                                      (cons (plist-get spec :ietf)
                                            (plist-get spec :org)))
                                    koek-ys/languages)))

  (defun koek-ys/make-ensure-result-dir (name)
    "Return function to ensure result directory of current code block exists.
NAME is a string, the variable name storing the result file
name."
    (lambda ()
      (when-let ((file-name
                  (file-name-directory
                   (or (koek-org/get-code-block-var-value name) ""))))
        (make-directory file-name 'parents))))
  :config
  ;; Load snippets
  (yas-reload-all)

  ;; Set new snippet file snippet
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name "snippet-mode/new" (car (last yas-snippet-dirs))))
    (setq yas-new-snippet-default
          (buffer-substring (re-search-forward (rx line-start "# --\n"))
                            (point-max))))
  :delight yas-minor-mode)

(use-package company
  :straight t
  :bind
  (:map company-mode-map
   ;; TAB or C-i (terminal)
   ([remap indent-for-tab-command] . company-indent-or-complete-common)
   ;; M-TAB or C-M-i (terminal)
   ([remap completion-at-point] . company-complete-common)
   :map company-active-map
   ("C-n" . company-select-next)
   ("C-p" . company-select-previous))
  :hook ((prog-mode conf-mode comint-mode cider-repl-mode) . company-mode)
  :preface
  (defmacro koek-cpny/set-backends (modes backends)
    "Set BACKENDS in MODES.
MODES is a list of symbols, the modes to set backends in.
BACKENDS evaluates to a list of backends, the backends to set,
see `company-backends'."
    (declare (indent 1))
    (let ((value-sym (gensym)))
      `(let ((,value-sym ,backends))
         ,@(seq-mapcat
            (lambda (mode)
              (let* ((prefix "koek-cpny/")
                     (mode-name (symbol-name mode))
                     (backends-sym
                      (intern (concat prefix mode-name "-backends")))
                     (f-sym
                      (intern (concat prefix "setup-" mode-name "-backends")))
                     (hook-sym (intern (concat mode-name "-hook"))))
                `((defvar ,backends-sym (copy-tree ,value-sym)
                    ,(format "List of backends in `%s'." mode))

                  (defun ,f-sym ()
                    ,(format "Setup backends in `%s'." mode)
                    (setq-local company-backends ,backends-sym))

                  (add-hook ',hook-sym #',f-sym))))
            modes))))

  (koek-cpny/set-backends (indium-repl-mode)
    '((company-indium-repl company-files :with company-yasnippet)))
  (koek-cpny/set-backends (geiser-mode geiser-repl-mode)
    '((geiser-company-backend company-files :with company-yasnippet)))
  (koek-cpny/set-backends (cmake-mode conf-mode makefile-mode scad-mode)
    '((company-dabbrev-code company-files :with company-yasnippet)))
  :config
  (setq company-backends
        '((company-capf company-files :with company-yasnippet)))
  (setq company-idle-delay nil)
  (setq company-show-quick-access t)
  :delight)

(use-package company-dabbrev
  :defer t
  :config
  (setq company-dabbrev-other-buffers t) ; Same major mode
  (setq company-dabbrev-ignore-case t) ; Case typed, during candidate collection
  (setq company-dabbrev-downcase nil)) ; Case candidate, when inserted

(use-package company-flx
  :straight t
  :after company
  :config
  (company-flx-mode))

(use-package help-mode
  :defer t
  :config
  (use-package link-hint
    :bind
    (:map help-mode-map
     ("j" . link-hint-open-link))))

(use-package help-fns
  :bind
  ("C-c d f" . describe-face))

(use-package descr-text
  :bind
  ("C-c d c" . describe-char))

(use-package helpful
  :straight t
  :bind
  (([remap describe-variable] . helpful-variable)
   ([remap describe-function] . helpful-callable)
   ([remap describe-key]      . helpful-key))
  :config
  (use-package link-hint
    :bind
    (:map helpful-mode-map
     ("j" . link-hint-open-link)))

  (setq helpful-max-buffers nil))

(use-package elisp-demos
  :straight t
  :after helpful
  :config
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))

(use-package info
  :bind
  ("C-c d i" . info-apropos)
  :config
  (use-package link-hint
    :bind
    (:map Info-mode-map
     ("j" . link-hint-open-link))))

(use-package man
  :bind
  ("C-c d m" . man))

(use-package apropos
  :bind
  ("C-c d a" . apropos)
  :config
  (use-package link-hint
    :bind
    (:map apropos-mode-map
     ("j" . link-hint-open-link))))

(use-package devdocs-lookup
  :straight (:host github :repo "skeeto/devdocs-lookup")
  :bind
  ("C-c d d" . devdocs-lookup)
  :preface
  (let ((specs '(("C"            . "c")
                 ("C++"          . "cpp")
                 ("OpenJDK"      . "openjdk~15")
                 ("Clojure"      . "clojure~1.10")
                 ("Erlang"       . "erlang~24")
                 ("HTML"         . "html")
                 ("CSS"          . "css")
                 ("JavaScript"   . "javascript")
                 ("DOM"          . "dom")
                 ("jQuery"       . "jquery")
                 ("lodash"       . "lodash~4")
                 ("Node.js"      . "node")
                 ("npm"          . "npm")
                 ("Express"      . "express")
                 ("Octave"       . "octave")
                 ("Python"       . "python~3.9")
                 ("NumPy"        . "numpy~1.20")
                 ("pandas"       . "pandas~1")
                 ("StatsModels"  . "statsmodels")
                 ("scikit-learn" . "scikit_learn")
                 ("scikit-image" . "scikit_image")
                 ("TensorFlow"   . "tensorflow~2.4")
                 ("Matplotlib"   . "matplotlib~3.4")
                 ("PostgreSQL"   . "postgresql~13"))))
    (dolist (spec specs)
      (pcase-let* ((`(,name . ,id) spec)
                   (symbol
                    (thread-last id
                      (replace-regexp-in-string
                       (rx "~" (one-or-more (any digit ".")) line-end) "")
                      (replace-regexp-in-string "_" "-")
                      (concat "koek-dl/lookup-")
                      intern)))
        (defalias symbol
          (lambda ()
            (interactive)
            (require 'devdocs-lookup)
            (devdocs-lookup id (devdocs-read-entry id)))
          (format "Lookup documentation for %s on DevDocs." name))))))

(use-package eldoc
  :straight t
  :bind
  ("C-c d e" . eldoc-doc-buffer)
  :config
  (setq eldoc-echo-area-use-multiline-p nil)
  :delight)

(use-package vterm
  :straight t
  :bind
  ("C-c x t" . vterm)
  :config
  ;; Resolve keybinding conflict with repeat
  (push "C-z" vterm-keymap-exceptions)
  (vterm--exclude-keys vterm-mode-map vterm-keymap-exceptions)
  (bind-key "C-c C-z" #'vterm-send-C-z vterm-mode-map))

(use-package eshell
  :bind
  ("C-c x e" . eshell))

(use-package esh-module
  :defer t
  :config
  (push 'eshell-smart eshell-modules-list))

(use-package em-unix
  :defer t
  :config
  (setq eshell-mv-interactive-query t)
  (setq eshell-cp-interactive-query t)
  (setq eshell-ln-interactive-query t)
  (setq eshell-rm-interactive-query t))

(use-package proced
  :bind
  ("C-c x p" . proced))

(use-package compile
  :bind
  ("C-c x c" . compile)
  :preface
  (defun koek-cmpl/style-output ()
    "Style process output.
Output is between `compilation-filter-start' and point."
    (require 'ansi-color)
    (ansi-color-apply-on-region compilation-filter-start (point)))
  :config
  (setq compilation-scroll-output 'first-error)
  (add-hook 'compilation-filter-hook #'koek-cmpl/style-output))

(use-package eww
  :bind
  ("C-c x b" . eww)
  :preface
  (defvar koek-eww/redirect-fs nil
    "List of redirect functions.
A redirect function redirects a URL.  It's passed a URL struct,
the URL to redirect, which it's allowed to modify.  For URL
fields, see `url-generic-parse-url'.  The redirect function must
return a redirected URL, a string, or, to not redirect the URL,
nil.")

  (defun koek-eww/redirect-reddit (url)
    "Redirect Reddit to Reddit mobile.
URL is a URL struct, the URL to redirect."
    (when (string-match-p
           (rx line-start
               (zero-or-one (or "www" "old") ".") "reddit.com"
               line-end)
           (or (url-host url) ""))
      (setf (url-host url) "i.reddit.com")
      (url-recreate-url url)))

  (defun koek-eww/redirect (url)
    "Redirect URL.
URL is a string, the URL to redirect.  The redirected URL is the
result of the first redirect function to return a URL or, when
none return a URL, nil.  For redirect functions, see
`koek-eww/redirect-fs'."
    (require 'url)
    (let ((expanded (eww--dwim-expand-url url)))
      (thread-last koek-eww/redirect-fs
        (mapcar (lambda (f)
                  (funcall f (url-generic-parse-url expanded))))
        (seq-find #'identity))))

  (define-advice eww (:filter-args (args) koek-eww/redirect-url)
    (let ((url (car args)))
      (cons (or (koek-eww/redirect url) url) (cdr args))))
  :config
  (use-package link-hint
    :bind
    (:map eww-mode-map
     ("j" . link-hint-open-link)))

  (push #'koek-eww/redirect-reddit koek-eww/redirect-fs))

(use-package shr
  :defer t
  :config
  (setq shr-use-colors nil)
  (setq shr-max-image-proportion 0.6)
  (setq shr-image-animate nil))

(use-package mu4e
  :bind
  ("C-c x m" . mu4e)
  :init
  (bind-key "C-c x C-m" #'compose-mail)

  (setq mail-user-agent 'mu4e-user-agent))

(use-package mu4e-vars
  :defer t
  :config
  (setq mu4e-get-mail-command
        (format "mbsync -c %s -a"
                (expand-file-name "isync/mbsyncrc" (xdg-config-home))))
  ;; When moving e-mail, isync expects them to be renamed
  (setq mu4e-change-filenames-when-moving t)

  (setq mu4e-completing-read-function #'completing-read)
  (setq mu4e-context-policy 'pick-first)
  (setq mu4e-compose-complete-addresses nil)

  (setq mu4e-use-fancy-chars t)
  (setq mu4e-hide-index-messages t))

(use-package mu4e-main
  :defer t
  :config
  (use-package mu4e-utils
    :bind
    (:map mu4e-main-mode-map
     ("G" . mu4e-update-mail-and-index)))) ; Mirror elfeed

(use-package mu4e-headers
  :defer t
  :config
  (use-package mu4e-utils
    :bind
    (:map mu4e-headers-mode-map
     ("G" . mu4e-update-mail-and-index)))

  (use-package mu4e-org
    :bind
    (:map mu4e-headers-mode-map
     ("C-c o c" . mu4e-org-store-and-capture)))

  ;; Headers must at least be the width of the header short name plus
  ;; two for sort direction. For header short names, see
  ;; `mu4e-header-info'.
  (setq mu4e-headers-fields '((:human-date . 8)
                              (:from       . 22)
                              (:subject    . nil)))

  ;; Style thread segments
  (let ((specs '((mu4e-headers-thread-child-prefix         . "├─")
                 (mu4e-headers-thread-last-child-prefix    . "└─")
                 (mu4e-headers-thread-connection-prefix    . "│ ")
                 (mu4e-headers-thread-blank-prefix         . "  ")
                 (mu4e-headers-thread-orphan-prefix        . "┌─")
                 (mu4e-headers-thread-single-orphan-prefix . "╶─")
                 (mu4e-headers-thread-duplicate-prefix     . "= "))))
    (dolist (spec specs)
      (pcase-let ((`(,symbol . ,segment) spec))
        (set symbol (cons segment segment))))))

(use-package mu4e-mark
  :defer t
  :config
  ;; When trashing e-mail, e-mail is flagged trashed. E-mail flagged
  ;; trashed is deleted by most e-mail providers. Move to trash but
  ;; don't flag trashed.
  (setq mu4e-marks
        (cons (cons 'trash
                    (plist-put (alist-get 'trash mu4e-marks)
                               :action
                               (lambda (docid _msg target)
                                 (mu4e~proc-move docid
                                                 (mu4e~mark-check-target target)
                                                 "-N"))))
              (assq-delete-all 'trash mu4e-marks)))

  ;; Style marker characters
  (setq mu4e-marks
        (mapcar (pcase-lambda (`(,mark . ,props))
                  (let ((marker (let ((spec (plist-get props :char)))
                                  (if (consp spec)
                                      (car spec)
                                    spec))))
                    (cons mark (plist-put props :char (cons marker marker)))))
                mu4e-marks)))

(use-package mu4e-view
  :defer t
  :config
  (use-package mu4e-utils
    :bind
    (:map mu4e-view-mode-map
     ("G" . mu4e-update-mail-and-index)))

  (use-package mu4e-actions
    :defer t
    :config
    (unless mu4e-msg2pdf
      (setq mu4e-view-actions
            (rassq-delete-all 'mu4e-action-view-as-pdf mu4e-view-actions))))

  (use-package mu4e-org
    :bind
    (:map mu4e-view-mode-map
     ("C-c o c" . mu4e-org-store-and-capture)))

  (add-to-list 'mu4e-view-actions
               '("open in browser" . mu4e-action-view-in-browser) 'append)
  (setq mu4e-view-show-addresses t))

;; Prefer variables of package mu4e-compose to those of message and
;; those of message to those of sendmail
(use-package message
  :defer t
  :preface
  (defun koek-msg/check-spelling ()
    "Check spelling of e-mail."
    (let ((ispell-skip-region-alist     ; Dynamic variable
           (let ((citation-line
                  (list
                   (rx line-start (one-or-more not-newline) "writes:" line-end)
                   #'forward-line))
                 (citation (list mu4e-cited-regexp #'forward-line)))
             (append (list citation-line citation) ispell-skip-region-alist)))
          (tick (buffer-chars-modified-tick)))
      (ispell-message)
      (unless (or (= (buffer-chars-modified-tick) tick)
                  (y-or-n-p "Spelling checked.  Send? "))
        (user-error "Send aborted"))))
  :config
  (setq message-send-mail-function #'smtpmail-send-it)
  (setq message-beginning-of-line nil)
  (add-hook 'message-send-hook #'koek-msg/check-spelling))

(use-package mu4e-compose
  :defer t
  :config
  ;; Revert remap
  (bind-keys
   :map mu4e-compose-mode-map
   ([remap beginning-of-buffer] . nil)
   ([remap end-of-buffer]       . nil))

  (setq mu4e-compose-context-policy 'ask-if-none))

(use-package mu4e-org
  :after org
  :config
  (require 'mu4e)

  (setq mu4e-org-link-query-in-headers-mode t))

(use-package bbdb
  :straight t
  :after mu4e
  :config
  (push '(("Belgium" "België") "spcC" "@%s\n@@%p @%c@\n%C@" "%c")
        bbdb-address-format-list)
  (setq bbdb-default-country nil)
  (setq bbdb-phone-style nil)
  (bbdb-initialize 'mu4e 'message))

(use-package bbdb-com
  :bind
  ("C-c x a" . bbdb))

(use-package bbdb-vcard
  :straight t
  :after bbdb
  :preface
  (defun koek-bbdb/import-dir (file-name &optional interactive)
    "Import vCards from directory FILE-NAME and its subdirectories.
INTERACTIVE is used internally."
    (interactive
     (let ((file-name
            (progn
              (require 'bbdb-vcard)
              (expand-file-name
               (read-directory-name
                "vCard directory: "
                (if (file-accessible-directory-p bbdb-vcard-default-dir)
                    bbdb-vcard-default-dir
                  default-directory)
                nil t)))))
       (list file-name 'interactive)))
    (require 'bbdb-vcard)
    (let* ((file-names
            (directory-files-recursively file-name (rx ".vcf" line-end) nil t))
           (vcards (with-temp-buffer
                     (dolist (file-name file-names)
                       (insert-file-contents file-name)
                       (when (and (eolp) (not (bolp)))
                         (insert "\n")))
                     (buffer-substring (point-min) (point-max))))
           (records
            (bbdb-vcard-iterate-vcards #'bbdb-vcard-import-vcard vcards))
           (n-records (length records)))
      (when interactive
        (message "%d %s imported"
                 n-records (if (= n-records 1) "vCard" "vCards")))))
  :config
  ;; Contacts sharing a landline telephone aren't duplicates
  (setq bbdb-vcard-try-merge nil))

(use-package bongo
  :straight t
  :bind
  (("C-c x k" . bongo)
   ("C-c k s" . bongo-seek)
   ("C-c k f" . bongo-seek-forward-10)
   ("C-c k b" . bongo-seek-backward-10)
   ("C-c k a" . bongo-replay-current)
   ("C-c k e" . bongo-perform-next-action)
   ("C-c k n" . bongo-play-next)
   ("C-c k p" . bongo-play-previous)
   ("C-c k x" . bongo-stop))
  :hook (dired-mode . bongo-dired-library-mode)
  :preface
  (define-advice bongo-default-library-buffer
      (:override () koek-bngo/get-default-library-buffer)
    (require 'dired)
    (dired-noselect bongo-default-directory))

  ;; Disable banner
  (define-advice bongo-default-playlist-buffer
      (:override () koek-bngo/get-default-playlist-buffer)
    (let ((buffer (get-buffer-create bongo-default-playlist-buffer-name)))
      (with-current-buffer buffer
        (unless (derived-mode-p 'bongo-playlist-mode)
          (bongo-playlist-mode)))
      buffer))

  (defun koek-bngo/play-pause ()
    "Pause or resume playback.
When playback is stopped, play from beginning."
    (interactive)
    (if (bongo-playing-p)
        (bongo-pause/resume)
      (with-bongo-playlist-buffer
        (save-excursion
          (goto-char (point-min))
          (bongo-play)))))

  (defun koek-bngo/enqueue (file-names &optional next)
    "Enqueue FILE-NAMES.
When NEXT is truthy, enqueue after playing track, else, enqueue
after last track."
    (with-temp-bongo-library-buffer
      (dolist (file-name file-names)
        (bongo-insert-file file-name))
      (bongo-enqueue-region (if next 'insert 'append)
                            (point-min) (point-max)
                            'maybe-display-playlist)))

  ;; Contrary to what its signature suggests,
  ;; `bongo-dired-enqueue-lines' enqueues only current line
  (defun koek-bngo/dired-enqueue-dwim (&optional arg)
    "Enqueue current line or marked lines.
With `\\[universal-argument]' prefix argument ARG, enqueue after
playing track, else, enqueue after last track."
    (interactive "P")
    (let* ((file-names (dired-get-marked-files))
           (mark-active
            (or (> (length file-names) 1)
                (eq (car (dired-get-marked-files nil nil nil 'distinguish))
                    t))))
      (koek-bngo/enqueue file-names arg)
      (unless mark-active
        (dired-next-line 1))))

  (defun koek-bngo/dired-enqueue-next-dwim ()
    "Enqueue current line or marked lines after playing track."
    (interactive)
    (koek-bngo/dired-enqueue-dwim t))
  :config
  ;; Resolve keybinding conflict with wdired
  (unbind-key "SPC" bongo-dired-library-mode-map)

  (bind-keys
   ("C-c k k" . koek-bngo/play-pause)
   :map bongo-dired-library-mode-map
   ([remap bongo-dired-append-enqueue-lines] . koek-bngo/dired-enqueue-dwim)
   ([remap bongo-dired-insert-enqueue-lines] . koek-bngo/dired-enqueue-next-dwim))

  ;; General
  (setq bongo-enabled-backends '(mpv))
  (setq bongo-custom-backend-matchers
        '((mpv . (local-file "m4a"))
          (mpv . ("https:" . t))))
  (setq bongo-prefer-library-buffers nil)
  (setq bongo-insert-whole-directory-trees t)
  (setq bongo-join-inserted-tracks nil)
  (setq bongo-display-playlist-after-enqueue nil)

  ;; Appearance
  (setq bongo-header-line-mode nil)
  (setq bongo-mode-line-indicator-mode nil)
  (setq bongo-mark-played-tracks t)
  (setq bongo-track-mark-icon-file-name nil)
  (setq bongo-display-track-icons nil)
  :delight bongo-dired-library-mode)

(use-package elfeed
  :straight t
  :bind
  ("C-c x n" . elfeed)
  :preface
  (defun koek-feed/get-entries ()
    "Return selected entries.
When called from show buffer, return current entry.  When called
from search buffer, return entries in region or entry on current
line."
    (if elfeed-show-entry
        (list elfeed-show-entry)
      (elfeed-search-selected)))

  (defun koek-feed/visit (entries)
    "Visit ENTRIES in eww."
    (require 'eww)
    (thread-last entries
      (mapcar (lambda (entry)
                (let ((buffer
                       (generate-new-buffer
                        (format "*eww: %s*" (elfeed-entry-title entry)))))
                  (with-current-buffer buffer
                    (eww-mode)
                    (eww (elfeed-entry-link entry)))
                  buffer)))
      (mapc #'pop-to-buffer-same-window)))

  (defun koek-feed/visit-dwim ()
    "Visit selected entries in eww."
    (interactive)
    (let ((entries (koek-feed/get-entries)))
      (when (derived-mode-p 'elfeed-search-mode)
        (elfeed-untag entries 'unread)
        (mapc #'elfeed-search-update-entry entries)
        (unless (or elfeed-search-remain-on-entry (use-region-p))
          (forward-line)))
      (koek-feed/visit entries)))

  (defun koek-feed/enqueue (entries &optional next)
    "Enqueue ENTRIES in bongo.
When NEXT is truthy, enqueue after playing track, else, enqueue
after last track."
    (require 'bongo)
    (with-temp-bongo-library-buffer
      (dolist (entry entries)
        (bongo-insert-uri (elfeed-entry-link entry)
                          (elfeed-entry-title entry)))
      (bongo-enqueue-region (if next 'insert 'append)
                            (point-min) (point-max)
                            'maybe-display-playlist)))

  (defun koek-feed/enqueue-dwim (&optional arg)
    "Enqueue selected entries in bongo.
With `\\[universal-argument]' prefix argument ARG, enqueue after
playing track, else, enqueue after last track."
    (interactive "P")
    (let ((entries (koek-feed/get-entries)))
      (when (derived-mode-p 'elfeed-search-mode)
        (elfeed-untag entries 'unread)
        (mapc #'elfeed-search-update-entry entries)
        (unless (or elfeed-search-remain-on-entry (use-region-p))
          (forward-line)))
      (koek-feed/enqueue entries arg)))

  (defun koek-feed/enqueue-next-dwim ()
    "Enqueue selected entries in bongo after playing track."
    (interactive)
    (koek-feed/enqueue-dwim t)))

(use-package elfeed-search
  :defer t
  :config
  (bind-keys
   :map elfeed-search-mode-map
   ("b" . koek-feed/visit-dwim)
   ("B" . elfeed-search-browse-url)
   ("e" . koek-feed/enqueue-dwim)
   ("E" . koek-feed/enqueue-next-dwim))

  (setq elfeed-search-filter (concat elfeed-search-filter " ")))

(use-package elfeed-show
  :defer t
  :config
  (use-package link-hint
    :bind
    (:map elfeed-show-mode-map
     ("j" . link-hint-open-link)))

  (bind-keys
   :map elfeed-show-mode-map
   ("b" . koek-feed/visit-dwim)
   ("B" . elfeed-show-visit)
   ("e" . koek-feed/enqueue-dwim)
   ("E" . koek-feed/enqueue-next-dwim)))

(use-package pdf-tools
  :straight t
  :defer t
  :preface
  (defun koek-pdf/recompile (&optional interactive)
    "Recompile epdf.
INTERACTIVE is used internally."
    (interactive (list 'interactive))
    (require 'pdf-tools)
    (let* (buffer
           (callback
            (lambda (file-name)
              (setq pdf-info-epdfinfo-program file-name)
              (when (and file-name buffer)
                (kill-buffer buffer))
              (when interactive
                (if file-name
                    (message "Recompiling epdf...done")
                  (message "Recompile epdf failed"))))))
      (when interactive
        (message "Recompiling epdf..."))
      (setq buffer
            (pdf-tools-build-server pdf-tools-directory
                                    'no-install-deps nil
                                    callback)))))

(use-package pdf-loader
  :config
  (pdf-loader-install 'no-ask 'no-install-deps))

(use-package pdf-view
  :mode ((rx ".pdf" string-end) . pdf-view-mode)
  :preface
  (defun koek-pdf/redraw-hard ()
    "Invalidate cache and redraw document."
    (pdf-cache-clear-images)
    (pdf-view-redisplay t))

  (defun koek-pdf/stain ()
    "Stain document."
    (pdf-info-setoptions :render/foreground (car pdf-view-midnight-colors)
                         :render/background (cdr pdf-view-midnight-colors)
                         :render/usecolors t)
    (koek-pdf/redraw-hard))

  ;; `pdf-view-midnight-minor-mode' is difficult to extend
  (define-minor-mode koek-pdf/stain-mode
    "Stain document for reading under different light conditions."
    :lighter " Stain"
    (unless (derived-mode-p 'pdf-view-mode)
      (user-error "Not a PDF buffer"))
    (if koek-pdf/stain-mode
        (progn
          (add-hook 'after-save-hook #'koek-pdf/stain nil 'local)
          (add-hook 'after-revert-hook #'koek-pdf/stain nil 'local)
          (koek-pdf/stain))
      (remove-hook 'after-save-hook #'koek-pdf/stain 'local)
      (remove-hook 'after-revert-hook #'koek-pdf/stain 'local)
      (pdf-info-setoptions :render/usecolors nil)
      (koek-pdf/redraw-hard)))

  ;; Hints are cached, i.e., cache must be invalidated on theme change
  (defun koek-pdf/invalidate-all ()
    "Invalidate cache all documents."
    (save-current-buffer
      (dolist (buffer (buffer-list))
        (set-buffer buffer)
        (when (derived-mode-p 'pdf-view-mode)
          (pdf-cache-clear-images)))))

  (defun koek-pdf/restain-all ()
    "Restain all stained documents."
    (save-current-buffer
      (dolist (buffer (buffer-list))
        (set-buffer buffer)
        (when (and (derived-mode-p 'pdf-view-mode) koek-pdf/stain-mode)
          (koek-pdf/stain)))))
  :config
  (use-package consult
    :bind
    (:map pdf-view-mode-map
     ("d" . consult-imenu)))

  (bind-keys
   :map pdf-view-mode-map
   ([remap pdf-view-midnight-minor-mode] . koek-pdf/stain-mode)
   ("C-c e s" . koek-pdf/stain-mode))

  (setq-default pdf-view-display-size 'fit-page)
  ;; First invalidate, then restain. `add-hook' adds to the front. The
  ;; reverse invalidates the restained page.
  (add-hook 'koek-thm/enable-hook #'koek-pdf/restain-all)
  (add-hook 'koek-thm/enable-hook #'koek-pdf/invalidate-all)
  :delight (pdf-view-mode "PDF" :major))

(use-package pdf-links
  :bind
  (:map pdf-links-minor-mode-map
   ([remap link-hint-open-link] . pdf-links-action-perform)
   ("j" . pdf-links-action-perform))
  :preface
  ;; Show hints in minuscule
  (define-advice pdf-links-read-link-action--create-keys
      (:around (f &rest args) koek-pdf/downcase-hints)
    (mapcar (lambda (chars)
              (mapcar #'downcase chars))
            (apply f args)))

  (define-advice pdf-links-read-link-action--read-chars
      (:around (f prompt actions) koek-pdf/normalize-hints)
    (funcall f prompt
             (mapcar (pcase-lambda (`(,hint . ,link))
                       (cons (mapcar #'upcase hint)
                             link))
                     actions)))
  :config
  (let ((spec
         (seq-mapcat
          (lambda (name)
            (list name (face-attribute 'pdf-links-read-link name nil 'default)))
          '(:family :width :weight :slant))))
    (setq pdf-links-read-link-convert-commands
          `("-density"    "96"
            "-family"     ,(plist-get spec :family)
            "-stretch"    ,(thread-last (plist-get spec :width)
                             symbol-name
                             capitalize
                             (replace-regexp-in-string "-" ""))
            "-weight"     ,(pcase (plist-get spec :weight)
                             ('ultra-light "Thin")
                             ('extra-light "ExtraLight")
                             ('light       "Light")
                             ('semi-bold   "SemiBold")
                             ('bold        "Bold")
                             ('extra-bold  "ExtraBold")
                             ('ultra-bold  "Black")
                             (_weight      "Normal"))
            "-style"      ,(pcase (plist-get spec :slant)
                             ('italic  "Italic")
                             ('oblique "Oblique")
                             (_slant   "Normal"))
            "-pointsize"  "%P"
            "-fill"       "%f"
            "-undercolor" "%b"
            "-draw"       "text %X,%Y '%c'"))))

(use-package saveplace-pdf-view
  :straight t
  :after pdf-view)

(use-package dictionary
  :straight t
  :bind
  ("C-c x d" . dictionary-search)
  :config
  (use-package link-hint
    :bind
    (:map dictionary-mode-map
     ("j" . link-hint-open-link)))

  (bind-key "DEL" #'scroll-down 'dictionary-mode-map)

  (setq dictionary-create-buttons nil))

(use-package calendar
  :bind
  ("C-c x q" . calendar)                ; [Q]alendar [sic]
  :hook (calendar-today-visible . calendar-mark-today)
  :config
  (setq calendar-mark-holidays-flag t))

(use-package holidays
  :bind
  ("C-c x C-q" . list-holidays))

(use-package prepcast
  :koek t
  :defer t
  :config
  (setq prepcast-scale 1.5)
  :delight)

(use-package keycast
  :straight t
  :defer t
  :config
  (setq keycast-window-predicate #'moody-window-active-p)
  (setq keycast-insert-after 'keycast-marker)
  (setq keycast-separator-width 3)
  (setq keycast-remove-tail-elements nil))

(use-package text-mode
  :mode (rx (or ".txt" "/README" "/LICENSE") string-end)
  :preface
  (defvar koek-txt/insecure-modes
    '(sgml-mode                         ; mhtml-mode derives from sgml-mode
      snippet-mode)
    "List of major mode symbols.
Modes are insecure about being derived from text-mode.")

  (defvar koek-txt/confident-hook nil
    "Normal hook run after enabling text-mode or derived modes.
Modes are confident about being derived from text-mode.")

  (defun koek-txt/run-confident-hook ()
    "Run `koek-txt/confident-hook'."
    (unless (apply #'derived-mode-p koek-txt/insecure-modes)
      (run-hooks 'koek-txt/confident-hook)))
  :config
  (add-hook 'text-mode-hook #'koek-txt/run-confident-hook)
  :delight (text-mode "Txt" :major))

(use-package cc-mode
  :mode
  (((rx ".c" string-end) . c-mode)
   ((rx ".cpp" string-end) . c++-mode)
   ((rx ".java" string-end) . java-mode))
  :config
  (bind-keys
   :map c-mode-map
   ("C-x p i l" . koek-eglt/init-clangd)
   ("C-x p i c" . koek-cmke/init)
   ("C-c d d" . koek-dl/lookup-c)
   :map c++-mode-map
   ("C-x p i l" . koek-eglt/init-clangd)
   ("C-x p i c" . koek-cmke/init)
   ("C-c d d" . koek-dl/lookup-cpp)
   :map java-mode-map
   ("C-c d d" . koek-dl/lookup-openjdk))

  ;; Resolve keybinding conflict with company
  (unbind-key "TAB" c-mode-base-map))

(use-package cc-cmds
  :defer t
  :preface
  (define-advice c-update-modeline
      (:around (f) koek-cc/disable-update-mode-name)
    (let ((name mode-name))
      (funcall f)
      (setq mode-name name)
      (force-mode-line-update))))

(use-package cc-vars
  :defer t
  :config
  (setq c-default-style
        '((awk-mode  . "awk")
          (java-mode . "java")
          (other     . "stroustrup"))))

(use-package gino
  :load-path "lisp/gino"
  :commands gino-generate-project)

(use-package clojure-mode
  :straight t
  :mode
  (((rx ".clj" string-end) . clojure-mode)
   ((rx ".cljs" string-end) . clojurescript-mode)
   ((rx ".cljc" string-end) . clojurec-mode)
   ((rx ".edn" string-end) . clojure-mode))
  :config
  (bind-keys
   :map clojure-mode-map
   ("C-x p i k" . koek-kndr/init)
   ("C-c d d" . koek-dl/lookup-clojure)
   ("C-c d C-j" . koek-dl/lookup-openjdk)
   :map clojurescript-mode-map
   ("C-c d C-j" . koek-dl/lookup-javascript)
   ("C-c d C-d" . koek-dl/lookup-dom)
   ("C-c d C-n" . koek-dl/lookup-node))
  :delight
  (clojure-mode "Clj" :major)
  (clojurescript-mode "Cljs" :major)
  (clojurec-mode "Cljc" :major))

(use-package cider
  :straight t
  :after clojure-mode)

(use-package cider-mode
  :defer t
  :config
  (use-package cider-eval
    :bind
    (:map cider-mode-map
     ("C-c C-c" . cider-load-buffer)))

  ;; Resolve keybinding conflict with org
  (unbind-key "C-c C-k" cider-mode-map)
  :delight)

(use-package cider-common
  :defer t
  :config
  (setq cider-prompt-for-symbol nil))

(use-package cider-repl
  :defer t
  :config
  (bind-key "C-c d d" #'koek-dl/lookup-clojure 'cider-repl-mode-map)

  ;; Resolve keybinding conflict with company
  (unbind-key "TAB" cider-repl-mode-map)

  (setq cider-repl-use-pretty-printing t))

(use-package cmake-mode
  :straight t
  :mode ((rx (or ".cmake" "/CMakeLists.txt") string-end) . cmake-mode)
  :preface
  ;; source tree, relative
  (defun koek-cmke/init (root config &optional interactive)
    (interactive
     (let ((root (or (koek-proj/locate-root) (user-error "Not in a project")))
           (config
            (or (car (koek-proj/locate-configs "CMakeLists.txt" nil 'prompt))
                (user-error "No CMake configuration found"))))
       (list root config 'interactive)))
    (when interactive
      (message "Initializing CMake..."))
    (let* ((default-directory root)     ; Dynamic variable
           (result
            (progn
              (make-directory "build" 'no-error)
              (make-symbolic-link
               "build/compile_commands.json" "compile_commands.json" 'overwrite)
              (call-process "cmake" nil nil nil
                            "-S" config "-B" "build/"
                            "-D" "CMAKE_EXPORT_COMPILE_COMMANDS=ON"))))
      (unless (zerop result)
        (error "CMake returned %d" result)))
    (when interactive
      (message "Initializing CMake...done")))
  :config
  (bind-key "C-x p i c" #'koek-cmke/init cmake-mode-map))

(use-package lisp-mode
  :mode (rx ".lisp" string-end))

(use-package inf-lisp
  :after lisp-mode
  :config
  (setq inferior-lisp-program "sbcl"))

(use-package sly
  :straight t
  :load-path "straight/build/sly/contrib" ; Silence warnings
  :after lisp-mode)

(use-package sly-mrepl
  :defer t
  :config
  ;; Resolve keybinding conflict with company
  (unbind-key "TAB" sly-mrepl-mode-map))

(use-package conf-mode
  :mode (rx (or ".desktop" "cross.txt") string-end)
  :preface
  (defvar koek-conf/mode-names
    '((conf-colon-mode . "Conf:")
      (conf-desktop-mode . "Desktop")
      (conf-javaprop-mode . "Properties")
      (conf-ppd-mode . "PPD")
      (conf-space-mode . "Conf\N{MIDDLE DOT}")
      (conf-toml-mode . "TOML")
      (conf-unix-mode . "Unix")
      (conf-windows-mode . "INI")
      (conf-xdefaults-mode . "Xdefaults"))
    "Alist of Conf mode symbol to Conf mode name pairs.")

  (define-advice conf-mode-initialize
      (:after (&rest _args) koek-conf/update-mode-name)
    (setq mode-name
          '(:eval
            ;; When mode-name is evaluated during mode line update,
            ;; inhibit-mode-name-delight is unbound or false
            (if (not (bound-and-true-p inhibit-mode-name-delight))
                (alist-get major-mode koek-conf/mode-names)
              "Conf")))))

(use-package elisp-mode
  :mode ((rx ".el" string-end) . emacs-lisp-mode)
  :config
  (use-package pp
    :bind
    (:map emacs-lisp-mode-map
     ("C-c e e" . pp-eval-last-sexp)
     ("C-c e m" . pp-macroexpand-last-sexp)
     :map lisp-interaction-mode-map
     ("C-c e e" . pp-eval-last-sexp)
     ("C-c e m" . pp-macroexpand-last-sexp)))

  (use-package helpful
    :bind
    (:map emacs-lisp-mode-map
     ("C-c C-d" . helpful-at-point)
     :map lisp-interaction-mode-map
     ("C-c C-d" . helpful-at-point)))

  (bind-keys
   :map emacs-lisp-mode-map
   ("C-c C-c" . eval-buffer)
   :map lisp-interaction-mode-map
   ("C-c C-c" . eval-buffer))
  :delight (emacs-lisp-mode "El" :major))

(use-package checkdoc
  :defer t
  :config
  (setq checkdoc-package-keywords-flag t))

(use-package erlang
  :straight t
  :mode ((rx ".erl" string-end) . erlang-mode)
  :config
  (bind-key "C-c d d" #'koek-dl/lookup-erlang 'erlang-mode-map)

  ;; Set Erlang home
  (let* ((file-names
          (mapcar #'file-name-directory
                  (file-expand-wildcards "c:/Program Files/erl*/bin/erlc.exe")))
         ;; Dynamic variable, shadow Chocolatey shim
         (exec-path (append file-names exec-path)))
    (when-let ((file-name (executable-find "erlc")))
      (setq erlang-root-dir
            (thread-first file-name
              file-truename
              (locate-dominating-file "bin")))))
  :delight (erlang-mode "Erl" :major))

(use-package mhtml-mode
  :mode (rx (or ".htm" ".html") string-end)
  :config
  (bind-key "C-c d d" #'koek-dl/lookup-html 'mhtml-mode-map)
  :delight (mhtml-mode "HTML" :major))

(use-package css-mode
  :mode (rx ".css" string-end)
  :config
  (bind-key "C-c d d" #'koek-dl/lookup-css 'css-mode-map))

(use-package emmet-mode
  :straight t
  :bind
  (:map emmet-mode-keymap
   ("C-<tab>" . emmet-expand-line))     ; Why does <tab> work but TAB not?
  :hook (mhtml-mode css-mode)
  :config
  (setq emmet-self-closing-tag-style " /")
  :delight)

(use-package js
  :mode ((rx ".js" string-end) . js-mode)
  :config
  (bind-keys
   :map js-mode-map
   ("C-c d d" . koek-dl/lookup-javascript)
   ("C-c d C-d" . koek-dl/lookup-dom)
   ("C-c d C-j" . koek-dl/lookup-jquery)
   ("C-c d C-l" . koek-dl/lookup-lodash)
   ("C-c d C-n" . koek-dl/lookup-node)
   ("C-c d C-x" . koek-dl/lookup-express))

  ;; Resolve keybinding conflict with eglot
  (unbind-key "M-." js-mode-map)

  (setq js-enabled-frameworks '(javascript))
  :delight (js-mode "JS" :major))

;; Prevent indium from creating Chrome profile directory during
;; installation
(setq indium-chrome-data-dir nil)

(use-package indium-interaction
  :straight indium
  :hook (js-mode . indium-interaction-mode)
  :config
  ;; Resolve keybinding conflict with documentation keymap
  (unbind-key "C-c d" indium-interaction-mode-map)
  :delight)

(use-package indium-chrome
  :defer t
  :config
  (setq indium-chrome-data-dir
        (no-littering-expand-var-file-name "indium/chrome-profile/"))
  (make-directory indium-chrome-data-dir 'parents))

(use-package indium-repl
  :defer t
  :config
  ;; Resolve keybinding conflict with company
  (unbind-key "TAB" indium-repl-mode-map))

(use-package json-mode
  :straight t
  :mode (rx ".json" string-end)
  :config
  (bind-key "C-c d C-n" #'koek-dl/lookup-npm 'json-mode-map))

(use-package make-mode
  :mode ((rx "/Makefile" string-end) . makefile-gmake-mode)
  :delight
  (makefile-mode "Make" :major)
  (makefile-automake-mode "Automake" :major)
  (makefile-bsdmake-mode "BSDMake" :major)
  (makefile-gmake-mode "GMake" :major)
  (makefile-imake-mode "IMake" :major)
  (makefile-makepp-mode "Make++" :major))

(use-package markdown-mode
  :straight t
  :mode (rx ".md" string-end)
  :config
  (setq markdown-command "pandoc -s -f markdown -t html5")
  (setq markdown-use-pandoc-style-yaml-metadata t)
  (setq markdown-asymmetric-header t)
  :delight (markdown-mode "MD" :major))

(use-package meson-mode
  :straight t
  :mode (rx "meson.build" string-end))

(use-package octave
  :mode ((rx ".m" string-end) . octave-mode)
  :config
  (bind-key "C-c d d" #'koek-dl/lookup-octave 'octave-mode-map)

  ;; Insert MATLAB compatible comments
  (setq octave-comment-char ?%)
  (setq octave-comment-start (char-to-string octave-comment-char))
  (setq octave-block-comment-start
        (concat (make-string 2 octave-comment-char) " "))

  (setq octave-blink-matching-block nil)
  :delight (octave-mode "M" :major))

(use-package scad-mode
  :straight t
  :mode (rx ".scad" string-end)
  :config
  ;; Resolve smartparens' handlers not being called
  (unbind-key "<return>" scad-mode-map) ; Why does <return> work but RET not?

  (setq scad-indent-style "stroustrup"))

(use-package org
  :mode ((rx ".org" string-end) . org-mode)
  :bind
  (:map org-mode-map
   ("C-M-f" . org-forward-heading-same-level)
   ("C-M-b" . org-backward-heading-same-level)
   ("C-M-n" . org-next-visible-heading)
   ("C-M-p" . org-previous-visible-heading)
   ("C-M-a" . org-previous-block)
   ("C-M-e" . org-next-block))
  :hook
  ((window-setup . org-clock-persistence-insinuate) ; After initial buffer
   (org-mode . org-cdlatex-mode)
   (org-babel-after-execute . org-redisplay-inline-images))
  :config
  (use-package avy
    :bind
    (:map org-mode-map
     ("C-c j h" . avy-org-goto-heading-timer)))

  (use-package consult
    :bind
    (:map org-mode-map
     ([remap consult-imenu] . consult-org-heading)))

  (use-package org-clock
    :bind
    (:map org-mode-map
     ("C-c o i" . org-clock-in)))

  (use-package org-roam
    :bind
    (:map org-mode-map
     ("C-c o n" . org-roam-insert)
     ("C-c o C-n" . org-roam-insert-immediate)))

  (use-package org-roam-buffer
    :bind
    (:map org-mode-map
     ("C-c o b" . org-roam-buffer-toggle-display)))

  (use-package outline
    :bind
    (:map org-mode-map
     ("C-M-u" . outline-up-heading)))

  ;; General
  (push 'org-protocol org-modules)
  (push 'beamer org-export-backends)
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((C . t) ; C and C++
                                 (clojure . t)
                                 (lisp . t)
                                 (emacs-lisp . t)
                                 (java . t)
                                 (js . t)
                                 (octave . t)
                                 (python . t)
                                 (scheme . t)
                                 (sql . t)))
  (setq org-adapt-indentation nil)

  ;; Headings
  (setq org-startup-folded t)
  (setq org-todo-keywords
        '((sequence "TODO(t)" "STALLED(s@/!)" "|" "DONE(d!)" "ABANDONED(a@)")))
  (setq org-tags-column 0)

  ;; Appearance
  (setq org-hide-emphasis-markers t)
  (setq org-pretty-entities t)
  (setq org-highlight-latex-and-related '(native))
  (push '("" "listings" nil) org-latex-packages-alist)

  ;; Appearance - Headings
  (setq org-fontify-whole-heading-line t)
  (setq org-fontify-done-headline nil)
  (setq org-ellipsis "…")

  ;; Appearance - Images
  (setq org-startup-with-inline-images t)
  (let ((width (thread-last (display-monitor-attributes-list)
                 car                    ; Primary monitor
                 (alist-get 'geometry)  ; Arrangement/resolution
                 (nth 2))))             ; Width
    (setq org-image-actual-width (floor (* width (/ 1 5.0)))))

  ;; Appearance - LaTeX previews
  (setq org-startup-with-latex-preview t)
  (setq org-preview-latex-image-directory
        (no-littering-expand-var-file-name "org/latex-previews/"))
  (setq org-format-latex-options
        (plist-put org-format-latex-options :scale (/ 4 3.0)))
  :delight org-cdlatex-mode)

(use-package org-agenda
  :bind
  ("C-c o a" . org-agenda)
  :config
  (setq org-agenda-time-leading-zero t))

(use-package org-capture
  :bind
  (("C-c o c" . org-capture)
   ("C-c j s" . org-capture-goto-last-stored))
  :preface
  (defun koek-org/setup-tag-completion ()
    "Setup tag completion for current.
Candidates are collected from agenda files."
    (setq-local org-complete-tags-always-offer-all-agenda-tags t))
  :config
  (add-hook 'org-capture-mode-hook #'hack-local-variables)
  (add-hook 'org-capture-mode-hook #'koek-org/setup-tag-completion))

(use-package org-clock
  :bind
  (("C-c j c" . org-clock-goto)
   ("C-c o o" . org-clock-out)
   ("C-c o x" . org-clock-cancel))
  :config
  (setq org-clock-persist 'clock)

  (org-clock-load))

(use-package org-refile
  :defer t
  :config
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-use-outline-path 'buffer-name))

(use-package org-src
  :preface
  (define-advice org-src--construct-edit-buffer-name
      (:override (org-buffer-name _lang) koek-org/construct-edit-buffer-name)
    ;; Mirror helpful buffer names
    (format "*org-src: %s*" org-buffer-name))
  :delight)

(use-package ob-core
  :defer t
  :preface
  (defun koek-org/get-code-block-var-value (name)
    "Return value of variable NAME for current code block.
NAME is a string, the variable's name."
    (thread-last (org-babel-get-src-block-info 'light)
      (nth 2)                           ; Header arguments
      (seq-filter (pcase-lambda (`(,type))
                    (eq type :var)))
      (mapcar #'cdr)
      (mapcar
       (lambda (value)
         (let ((parts (split-string value "=")))
           (cons (car parts)
                 (replace-regexp-in-string
                  (rx (or (seq line-start "\"") (seq "\"" line-end))) ""
                  (string-join (cdr parts) "="))))))
      (seq-find (pcase-lambda (`(,nm))
                  (string-equal nm name)))
      cdr))
  :config
  ;; Evaluate code blocks in buffer after confirmation
  (let ((whitelist nil))
    (setq org-confirm-babel-evaluate
          (lambda (_language _body)
            ;; A reference to a buffer is a good id but would storing
            ;; a reference prevent the buffer from being garbage
            ;; collected?
            (let ((id (or (buffer-file-name) (buffer-name))))
              (unless (assoc id whitelist)
                (push (cons id
                            (yes-or-no-p
                             (format "Evaluate code blocks in %s on your system this session? "
                                     id)))
                      whitelist))
              (not (cdr (assoc id whitelist))))))))

(use-package ob-tangle
  :defer t
  :preface
  (define-advice org-babel-tangle
      (:around (f &rest args) koek-org/disable-recentf)
    ;; Dynamic variables
    (let ((find-file-hook (remq 'recentf-track-opened-file find-file-hook))
          (write-file-functions
           (remq 'recentf-track-opened-file write-file-functions)))
      (apply f args)))

  (defun koek-org/gen-autoloads ()
    "Generate autoloads for Emacs Lisp packages."
    (when (derived-mode-p 'emacs-lisp-mode)
      (require 'autoload)
      (let* ((file-name (buffer-file-name))
             (package-dir (file-name-directory file-name))
             (package-name (thread-first package-dir
                             directory-file-name
                             file-name-base)))
        (when (string-equal (file-name-base file-name) package-name)
          (let ((generated-autoload-file ; Dynamic variable
                 (expand-file-name (concat package-name "-autoloads.el")
                                   package-dir)))
            (update-directory-autoloads package-dir)
            (kill-buffer (find-buffer-visiting generated-autoload-file)))))))

  (defun koek-org/compile-emacs-lisp ()
    "Compile Emacs Lisp files."
    (when (derived-mode-p 'emacs-lisp-mode)
      (require 'bytecomp)
      (byte-recompile-file (buffer-file-name) nil 0)))

  (defun koek-org/process-file-end ()
    "Process end of tangled file.
Code blocks end with empty line.  When `require-final-newline' is
nil, delete empty line at end of file."
    (unless require-final-newline
      (save-excursion
        (goto-char (point-max))
        (unless (bobp)
          (delete-char -1)
          (save-buffer)))))
  :config
  (add-hook 'org-babel-post-tangle-hook #'koek-org/process-file-end)
  (add-hook 'org-babel-post-tangle-hook #'koek-org/compile-emacs-lisp)
  (add-hook 'org-babel-post-tangle-hook #'koek-org/gen-autoloads))

(use-package ob-clojure
  :defer t
  :config
  (setq org-babel-clojure-backend 'cider))

(use-package ol
  :bind
  ("C-c o l" . org-store-link)
  :config
  (setq org-link-keep-stored-after-insertion t))

(use-package ol-bbdb
  :defer t
  :preface
  (defun koek-org/construct-birthday-entry-name (name age _age-suffix)
    "Return name of agenda entry for birthday.
NAME is a string, the name of the person.  AGE is an integer, the
age of the person.  _AGE-SUFFIX is ignored."
    (format "[[bbdb:%s][%s (%d %s old)]]"
            name name age (if (= age 1) "year" "years")))
  :config
  ;; BBDB anniversary (many, any type) and vCard ANNIVERSARY (one, any
  ;; type except birthday) aren't compatible, birthday and BDAY are
  (setq org-bbdb-anniversary-field 'birthday)
  (setq org-bbdb-anniversary-format-alist
        '(("birthday" . koek-org/construct-birthday-entry-name))))

(straight-use-package 'htmlize)         ; Optional dependency

(use-package ox
  :defer t
  :config
  (setq org-export-time-stamp-file nil)
  (setq org-export-headline-levels 4)
  (setq org-export-with-date nil)
  (setq org-export-with-toc 3))

(use-package ox-html
  :defer t
  :config
  (setq org-html-doctype "html5")
  (setq org-html-html5-fancy t)
  (setq org-html-validation-link nil))

(use-package ox-latex
  :defer t
  :config
  (setq org-latex-pdf-process '("latexmk -pdf %f"))
  (setq org-latex-listings t)

  ;; Setup hyperref
  (let ((options
         (let ((normalized (replace-regexp-in-string
                            (rx (any " \n")) "" org-latex-hyperref-template)))
           (string-match
            (rx "{" (group-n 1 (one-or-more (any alpha "={}%,"))) "}")
            normalized)
           (split-string (match-string 1 normalized) ","))))
    (setq org-latex-hyperref-template
          (concat "\\hypersetup{\n "
                  (string-join (append options '("hidelinks")) ",\n ")
                  "\n}\n"))))

(use-package org-roam
  :straight t
  :bind
  ("C-c o f" . org-roam-find-file)
  :preface
  (defun koek-org/title-to-slug (title)
    "Convert note title TITLE to a file name slug.
TITLE is a string, a note title."
    (let ((parts (split-string
                  (replace-regexp-in-string (rx (not (any alnum))) " " title))))
      (downcase (string-join parts "-"))))
  :config
  (setq org-roam-title-to-slug-function #'koek-org/title-to-slug)
  :delight)

(use-package org-roam-db
  :defer t
  :config
  (setq org-roam-db-location (no-littering-expand-var-file-name "org-roam.db")))

(use-package org-roam-protocol
  :after org-protocol)

(use-package org-caldav
  :straight t
  :bind
  ("C-c o s" . org-caldav-sync))

(use-package python
  :mode ((rx ".py" string-end) . python-mode)
  :preface
  (defun koek-py/disable-checker ()
    "Disable Python checker for current."
    (remove-hook 'flymake-diagnostic-functions #'python-flymake 'local))
  :config
  (bind-keys
   :map python-mode-map
   ("C-c d d" . koek-dl/lookup-python)
   ("C-c d C-n" . koek-dl/lookup-numpy)
   ("C-c d C-p" . koek-dl/lookup-pandas)
   ("C-c d C-s" . koek-dl/lookup-statsmodels)
   ("C-c d C-l" . koek-dl/lookup-scikit-learn)
   ("C-c d C-i" . koek-dl/lookup-scikit-image)
   ("C-c d C-t" . koek-dl/lookup-tensorflow)
   ("C-c d C-m" . koek-dl/lookup-matplotlib))

  (add-hook 'python-mode-hook #'koek-py/disable-checker)
  :delight (python-mode "Py" :major))

(use-package pipenv
  :straight t
  :hook (python-mode . pipenv-mode)
  :delight)

(use-package scheme
  :mode ((rx ".scm" string-end) . scheme-mode)
  :delight
  (scheme-mode
   (:eval
    (if geiser-impl--implementation
        (capitalize (symbol-name geiser-impl--implementation))
      "Scm"))
   :major))

(use-package geiser
  :straight t
  :after scheme)

(use-package geiser-autodoc
  :defer t
  :delight)

(use-package geiser-company
  :defer t
  :preface
  (define-advice geiser-company--setup-company
      (:around (f &rest args) koek-gsr/disable-setup-backends)
    (let ((backends company-backends))
      (apply f args)
      (setq company-backends backends))))

(use-package geiser-impl
  :defer t
  :config
  (setq geiser-default-implementation 'guile))

(use-package geiser-mode
  :defer t
  :delight)

(use-package geiser-repl
  :defer t
  :config
  ;; Resolve keybinding conflict with company
  (unbind-key "TAB" geiser-repl-mode-map)

  (setq geiser-repl-query-on-exit-p t))

(use-package sql
  :mode ((rx ".sql" string-end) . sql-mode)
  :preface
  ;; When SQL dialect is setup, whitespace-mode faces are overridden
  (define-advice sql-mode (:around (f) koek-sql/defer-whitespace-mode)
    (let ((prog-mode-hook               ; Dynamic variable
           (remq 'whitespace-mode prog-mode-hook)))
      (funcall f)))

  (define-advice sql-highlight-product
      (:around (f) koek-sql/re-enable-whitespace-mode)
    (whitespace-mode 0)
    (funcall f)
    (whitespace-mode))

  ;; When SQL dialect is setup, mode-name is overridden, i.e., delight
  ;; is undone
  (define-advice sql-highlight-product (:after () koek-sql/update-mode-name)
    (setq mode-name
          '(:eval
            (if (or (bound-and-true-p inhibit-mode-name-delight)
                    (eq sql-product 'ansi))
                "SQL"
              (sql-get-product-feature sql-product :name)))))
  :config
  (bind-key "C-c d d" #'koek-dl/lookup-postgresql 'sql-mode-map)

  ;; Upcase keywords after insertion
  (require 'find-func)
  (require 'abbrev)

  (let ((keywords
         (split-string
          (with-temp-buffer
            (insert-file-contents (find-library-name "sql"))
            (search-forward "ANSI Reserved keywords")
            (buffer-substring (re-search-forward
                               (rx (one-or-more (not (any "\"")))))
                              (re-search-forward
                               (rx (one-or-more (not (any ")")))))))
          (rx (any " \n")) 'omit-nulls "\"")))
    (dolist (keyword keywords)
      (define-abbrev
        sql-mode-abbrev-table keyword (upcase keyword) nil :system t)))

  (setq sql-product 'postgres))

(use-package sql-indent
  :straight t
  :hook (sql-mode . sqlind-minor-mode)
  :delight sqlind-minor-mode)

(use-package tex
  :straight auctex
  :mode ((rx ".tex" string-end) . TeX-tex-mode))

(use-package cdlatex
  :straight t
  :hook (LaTeX-mode . cdlatex-mode)
  :delight)

(use-package wolfram-mode
  :straight t
  :mode (rx ".wl" string-end)
  :delight (wolfram-mode "WL" :major))

(use-package yasnippet
  :mode ("/snippets/" . snippet-mode))

(setq frame-title-format
      '((:eval
         (let ((root (koek-proj/get-root))
               (file-name (buffer-file-name)))
           (cond
            ((and root file-name)
             (format "~%s/%s"
                     (koek-proj/get-name) (file-relative-name file-name root)))
            (file-name
             (abbreviate-file-name file-name))
            (t
             "%b"))))
        " - Emacs"))

(defvar koek-thm/enable-hook nil
  "Normal hook run after enabling theme.")

(define-advice enable-theme (:after (&rest _args) koek-thm/run-enable-hook)
  (run-hooks 'koek-thm/enable-hook))

(defvar koek-thm/dark-themes nil
  "List of theme symbols.
Themes are dark themes.")

(defun koek-thm/darkp (&optional theme)
  "Return whether THEME is a dark theme.
Optional THEME is a symbol, the theme to interrogate and defaults
to the current theme."
  (memq (or theme (car custom-enabled-themes)) koek-thm/dark-themes))

(defun koek-thm/set-frame-theme-variant (frame)
  "Set theme variant of FRAME.
When current theme is a dark theme, set frame theme variant to
dark, else, clear frame theme variant."
  (when (fboundp 'koek-thm/set-frame-theme-variant-xprop)
    (koek-thm/set-frame-theme-variant-xprop frame)))

(when (executable-find "xprop")
  (defun koek-thm/set-frame-theme-variant-xprop (frame)
    "Set theme variant of FRAME.
Mustn't be called directly, see
`koek-thm/set-frame-theme-variant'."
    (call-process "xprop" nil 0 nil
                  "-id" (frame-parameter frame 'outer-window-id)
                  "-f" "_GTK_THEME_VARIANT" "8u"
                  "-set" "_GTK_THEME_VARIANT" (if (koek-thm/darkp) "dark" ""))))

(defun koek-thm/update-frame-theme-variant ()
  "Update theme variant of all frames."
  (mapc #'koek-thm/set-frame-theme-variant (frame-list)))

(add-hook 'after-make-frame-functions #'koek-thm/set-frame-theme-variant)
(add-hook 'koek-thm/enable-hook #'koek-thm/update-frame-theme-variant)

(use-package modus-themes
  :straight t
  :preface
  (defun koek-mt/load (variant)
    "Load and enable Modus theme variant VARIANT.
VARIANT is a symbol, the Modus theme variant, either operandi or
vivendi."
    (pcase-let* ((koek-thm/enable-hook nil) ; Dynamic variable
                 (themes '(modus-operandi modus-vivendi))
                 (`(,new ,old)
                  (if (eq variant 'operandi) themes (reverse themes))))
      (when (custom-theme-enabled-p old)
        (disable-theme old))
      (unless (custom-theme-p new)
        (load-theme new 'no-confirm 'no-enable))
      (enable-theme new)
      (modus-themes-with-colors
        (custom-set-faces
         `(koek-diff/variant            ((,class :inherit bold)))
         `(eyebrowse-mode-line-active   ((,class :foreground unspecified)))
         `(eyebrowse-mode-line-inactive ((,class :foreground ,bg-alt)))
         `(koek-wm/selected-workspace   ((,class :inherit bold)))
         `(koek-wm/unselected-workspace ((,class :foreground ,bg-alt)))
         `(pdf-links-read-link          ((,class
                                          :foreground unspecified :background unspecified
                                          :inherit (modus-themes-intense-magenta bold)))))))
    ;; After user theme
    (run-hooks 'koek-thm/enable-hook))

  (defun koek-mt/toggle-variant ()
    "Toggle Modus theme variant."
    (interactive)
    (koek-mt/load
     (if (eq (car custom-enabled-themes) 'modus-operandi) 'vivendi 'operandi)))
  :init
  (bind-key "C-c z t" #'koek-mt/toggle-variant)

  (setq modus-themes-bold-constructs t)
  (setq modus-themes-slanted-constructs t)
  (setq modus-themes-hl-line 'underline-accented)
  (setq modus-themes-subtle-line-numbers t)
  (setq modus-themes-mode-line 'moody)
  (setq modus-themes-headings '((t . section)))
  (setq modus-themes-variable-pitch-headings t)
  (setq modus-themes-scale-headings t)
  (setq modus-themes-diffs 'deuteranopia)
  (setq modus-themes-org-blocks 'greyscale)
  :config
  (setq face-near-same-color-threshold 45000)
  (push 'modus-vivendi koek-thm/dark-themes)
  (koek-mt/load 'vivendi))

(defvar koek-font/pairs
  '(((:family "PragmataPro Liga" :height 110)
     (:family "IBM Plex Sans" :height 1.0))
    ((:family "Iosevka" :height 110)
     (:family "IBM Plex Sans" :height 1.0)))
  "List of font pairs.
A font pair is a list of two font specifications, one for fixed
pitch and one for variable pitch faces.  A font specification is
a plist of face attributes, see `set-face-attribute'.  Both font
specifications must set the family face attribute.  The fixed
pitch font specification must set an absolute height, the
variable pitch optionally a relative height.")

(when-let* ((pair
             (let ((families (seq-uniq (font-family-list))))
               (seq-find
                (lambda (pair)
                  (seq-every-p (lambda (spec)
                                 (member (plist-get spec :family) families))
                               pair))
                koek-font/pairs)))
            (fixed (car pair))
            (variable (cadr pair)))
  (apply #'set-face-attribute 'default nil fixed)
  (apply #'set-face-attribute
         'fixed-pitch nil (plist-put (copy-sequence fixed) :height 1.0))
  (apply #'set-face-attribute 'variable-pitch nil variable))

(setq window-divider-default-right-width 1)
(window-divider-mode)

(use-package moody
  :straight t
  :preface
  (defconst koek-ml/separator (make-string 3 ?\s)
    "Mode line group separator.")

  (defconst koek-ml/large-separator
    (make-string (* (length koek-ml/separator) 5) ?\s)
    "Mode line left right separator.")

  (defconst koek-ml/dummies '((eldoc-mode-line-string nil))
    "Dummies mode line construct.
A dummy prevents a package from modifying the mode line.")

  (defconst koek-ml/eldoc
    '(eldoc-mode-line-string ("" eldoc-mode-line-string koek-ml/separator))
    "Eldoc mode line construct.")

  (defun koek-ml/get-window-label ()
    "Return window label of selected window."
    (when-let ((label (window-parameter nil 'ace-window-path)))
      (substring-no-properties label)))

  (defconst koek-ml/ace
    '(:eval
      (when (bound-and-true-p ace-window-mode)
        (when-let ((label (koek-ml/get-window-label)))
          `(,(moody-ribbon (propertize label 'face 'aw-mode-line-face) nil 'up)
            koek-ml/separator))))
    "Ace mode line construct.")

  (defvar-local koek-ml/variant nil
    "Ediff variant.")

  (defconst koek-ml/ediff
    '(:eval
      (when (and koek-ml/variant (not (bound-and-true-p ace-window-mode)))
        `(,(moody-ribbon
            (concat (propertize (plist-get koek-ml/variant :label)
                                'face 'koek-diff/variant)
                    (when-let ((state (plist-get koek-ml/variant :state)))
                      (concat " " state)))
            nil 'up)
          koek-ml/separator)))
    "Ediff mode line construct.")

  (defconst koek-ml/depth
    '(:eval
      (let ((depth (- (recursion-depth) (minibuffer-depth))))
        (when (and (> depth 0) (moody-window-active-p))
          `(,(moody-ribbon (format "[%d]" depth) nil 'up)
            koek-ml/separator))))
    "Recursive edit depth mode line construct.")

  (defvar koek-ml/roman-numerals
    '((9 . "IX")
      (5 . "V")
      (4 . "IV")
      (1 . "I"))
    "Alist of sorted Arabic numeral to Roman numeral pairs.")

  (defun koek-ml/arabic-to-roman (n &optional roman-numerals)
    "Convert Arabic number N to a Roman number.
N is an integer greater than zero.  ROMAN-NUMERALS is used
internally."
    (unless roman-numerals
      (setq roman-numerals koek-ml/roman-numerals))
    (when (> n 0)
      (pcase-let ((`(,arabic . ,roman) (car roman-numerals)))
        (if (>= n arabic)
            (concat roman (koek-ml/arabic-to-roman (- n arabic) roman-numerals))
          (koek-ml/arabic-to-roman n (cdr roman-numerals))))))

  (defun koek-ml/get-exwm-workspaces ()
    "Return workspaces of selected monitor."
    (thread-last (number-sequence 0 (1- (length exwm-workspace--workareas)))
      (seq-group-by (lambda (n)
                      (nth n exwm-workspace--workareas)))
      (mapcar #'cdr)
      (seq-find (lambda (ns)
                  (memq exwm-workspace-current-index ns)))
      (mapcar (lambda (n)
                (list :n n :label (or (koek-ml/arabic-to-roman n) "N"))))))

  (defconst koek-ml/exwm-workspaces
    '(:eval
      (when (and (boundp 'exwm-workspace-current-index) (moody-window-active-p))
        (let ((workspaces (koek-ml/get-exwm-workspaces)))
          (when (> (length workspaces) 1)
            `(,(moody-ribbon
                (mapconcat
                 (lambda (workspace)
                   (let ((face (if (= (plist-get workspace :n)
                                      exwm-workspace-current-index)
                                   'koek-wm/selected-workspace
                                 'koek-wm/unselected-workspace)))
                     (propertize (plist-get workspace :label) 'face face)))
                 workspaces " ")
                nil 'up)
              koek-ml/separator)))))
    "Exwm workspaces mode line construct.")

  (defun koek-ml/get-eyebrowse-workspaces ()
    "Return workspaces of selected frame."
    (mapcar (lambda (workspace)
              (let ((n (nth 0 workspace))
                    (name (let ((name (nth 2 workspace)))
                            (unless (string-equal name "")
                              name))))
                (list :n n
                      :label (concat (or (koek-ml/arabic-to-roman n) "N")
                                     (when name
                                       (concat ":" name))))))
            (eyebrowse--get 'window-configs)))

  (defconst koek-ml/eyebrowse
    '(:eval
      (when (and (bound-and-true-p eyebrowse-mode) (moody-window-active-p))
        (let ((workspaces (koek-ml/get-eyebrowse-workspaces))
              (selected-n (eyebrowse--get 'current-slot)))
          (when (or (> (length workspaces) 1) (/= selected-n 0))
            `(,(moody-ribbon
                (mapconcat
                 (lambda (workspace)
                   (let ((face (if (= (plist-get workspace :n) selected-n)
                                   'eyebrowse-mode-line-active
                                 'eyebrowse-mode-line-inactive)))
                     (propertize (plist-get workspace :label) 'face face)))
                 workspaces " ")
                nil 'up)
              koek-ml/separator)))))
    "Eyebrowse mode line construct.")

  (defun koek-ml/truncate (s length)
    "Truncate string S to LENGTH.
S is a string, the string to truncate.  LENGTH is an integer, the
maximum length of S."
    (truncate-string-to-width s length nil nil t))

  (defconst koek-ml/id
    '(:eval
      (moody-tab
       (concat
        (when (derived-mode-p 'prog-mode 'conf-mode)
          (when-let ((name (koek-proj/get-name)))
            (concat (koek-ml/truncate name 16) "/")))
        (propertize (koek-ml/truncate (buffer-name) 32)
                    'face 'mode-line-buffer-id))))
    "Id mode line construct.")

  (defconst koek-ml/state '(" " "%*%+")
    "State mode line construct.")

  (defconst koek-ml/keycast
    '(:eval
      (when (bound-and-true-p keycast-mode)
        mode-line-keycast))
    "Keycast mode line construct.")

  (defconst koek-ml/position
    '(:eval
      (unless (derived-mode-p 'pdf-view-mode)
        `("" koek-ml/large-separator
          ,(when (buffer-narrowed-p)
             (list (moody-ribbon "Narrowed" nil 'up) " "))
          "%p" " " "%l,%c")))
    "Position mode line construct.")

  (defconst koek-ml/pdf
    '(:eval
      (when (derived-mode-p 'pdf-view-mode)
        `("" koek-ml/large-separator
          ,(format "%d/%d"
                   (pdf-view-current-page) (pdf-cache-number-of-pages)))))
    "PDF-tools mode line construct.")

  (defconst koek-ml/exwm-input
    '(:eval
      (when (and (boundp 'exwm--input-mode) (eq exwm--input-mode 'char-mode)
                 (moody-window-active-p))
        `("" koek-ml/separator
          ,(moody-ribbon "Char" nil 'up))))
    "Exwm input mode line construct.")

  (defconst koek-ml/input
    '(:eval
      (when (and current-input-method (moody-window-active-p))
        `("" koek-ml/separator
          ,(moody-ribbon current-input-method-title nil 'up))))
    "Input mode line construct.")

  (defvar koek-ml/checker-names
    '((eglot-flymake-backend . "LSP")
      (elisp-flymake-byte-compile . "El")
      (elisp-flymake-checkdoc . "CDoc")
      (flymake-kondor-backend . "Kondo"))
    "Alist of checker symbol to checker name pairs.")

  (defun koek-ml/state-to-description (state)
    "Convert checker state STATE to a description.
STATE is a symbol, a flymake state."
    (let ((words (split-string (symbol-name state) "-")))
      (string-join (cons (capitalize (car words)) (cdr words)) " ")))

  (defun koek-ml/get-flymake-state ()
    "Return state of flymake.
State is the symbol running (some checkers running),
finished (all checkers finished running), all-disabled (all
compatible checkers disabled) or no-checker (no compatible
checkers)."
    (let* ((enabled (flymake-running-backends))
           (finished (flymake-reporting-backends))
           (running (seq-filter (lambda (checker)
                                  (not (memq checker finished)))
                                enabled))
           (disabled (flymake-disabled-backends)))
      (cond
       (running
        'running)
       (finished
        'finished)
       (disabled
        'all-disabled)
       (t
        'no-checker))))

  (defun koek-ml/get-flymake-n-diags ()
    "Return number of diagnoses per error type."
    (thread-last (hash-table-values flymake--backend-state)
      (seq-mapcat #'flymake--backend-state-diags)
      (seq-group-by (lambda (diag)
                      (flymake--lookup-type-property (flymake--diag-type diag)
                                                     'flymake-category)))
      (mapcar (pcase-lambda (`(,cat . ,diags))
                (cons cat (length diags))))))

  (defconst koek-ml/flymake
    '(:eval
      (when (and (bound-and-true-p flymake-mode) (moody-window-active-p))
        `("" koek-ml/separator
          ,(when-let ((name
                       ;; First enabled checker
                       (alist-get (car (reverse (flymake-running-backends)))
                                  koek-ml/checker-names)))
             (concat name " "))
          ,(pcase (koek-ml/get-flymake-state)
             (`running
              "-;-;-")
             (`finished
              (let ((n-diags (koek-ml/get-flymake-n-diags)))
                (mapconcat
                 (lambda (cat)
                   (propertize
                    (number-to-string (alist-get cat n-diags 0))
                    'face (flymake--lookup-type-property cat 'mode-line-face)))
                 '(flymake-error flymake-warning flymake-note) ";")))
             (state
              (koek-ml/state-to-description state))))))
    "Flymake mode line construct.")

  (defconst koek-ml/vc
    '(:eval
      (when (and (bound-and-true-p vc-mode) (moody-window-active-p))
        (let ((state (string-trim (substring-no-properties vc-mode))))
          ;; For format, see `vc-default-mode-line-string'
          (string-match
           (rx (group-n 1 (one-or-more (not (any "-:@!?")))) (any "-:@!?")
               (zero-or-one (not (any ":")) ":")
               (group-n 2 (one-or-more not-newline)))
           state)
          `("" koek-ml/separator
            ,(format "%s %s" (match-string 1 state) (match-string 2 state))))))
    "Version control mode line construct.")

  (defconst koek-ml/task
    '(:eval
      (when (and (org-clock-is-active) (moody-window-active-p))
        `("" koek-ml/separator
          ,(org-duration-from-minutes (org-clock-get-clocked-time)))))
    "Task mode line construct.")

  (defconst koek-ml/modes
    '("" koek-ml/separator "(" mode-name mode-line-process minor-mode-alist ")")
    "Modes mode line construct.")

  ;; ediff
  (defconst koek-ml/diff
    '(:eval
      `("" koek-ml/large-separator
        ,(let ((diff-n (1+ ediff-current-difference))
               (n-diffs ediff-number-of-differences))
           (cond
            ((< diff-n 1)
             (format "Start -/%d" n-diffs))
            ((> diff-n n-diffs)
             (format "End -/%d" n-diffs))
            (t
             (format "%d/%d" diff-n n-diffs))))))
    "Ediff diff mode line construct.")

  (defvar koek-ml/variant-types '(A B C Ancestor)
    "List of variant types.")

  (defun koek-ml/get-variant-state (type)
    "Return state of variant type TYPE for current diff.
TYPE is a symbol, the variant type, see `koek-ml/variant-types'."
    (when (ediff-valid-difference-p)
      (let* ((diff
              (let ((diff
                     (if (eq type 'Ancestor)
                         (ediff-get-state-of-merge ediff-current-difference)
                       (ediff-get-state-of-diff ediff-current-difference
                                                type))))
                (pcase diff
                  ("prefer-A"
                   "=B")
                  ("prefer-B"
                   "=A")
                  ("=diff(A)"
                   "=A")
                  ("=diff(B)"
                   "=B")
                  ("=diff(C)"
                   "=C")
                  ("=diff(A+B)"
                   "=A+B"))))
             (merge (when (eq type 'C)
                      (ediff-get-state-of-merge ediff-current-difference)))
             (ancestor
              (when (eq type 'C)
                (and (ediff-get-state-of-ancestor ediff-current-difference)
                     "empty")))
             (state (concat diff
                            (when (and diff merge)
                              ";")
                            merge
                            (when (and (or diff merge) ancestor)
                              ";")
                            ancestor)))
        (unless (string-empty-p state)
          state))))

  (define-advice ediff-refresh-mode-lines
      (:override () koek-ml/update-mode-line)
    (setq mode-line-format
          `(,@koek-ml/dummies " "
            koek-ml/eldoc koek-ml/ace koek-ml/depth
            koek-ml/exwm-workspaces koek-ml/eyebrowse
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

  (add-hook 'ediff-cleanup-hook #'koek-ml/cleanup-variants)

  ;; calendar
  (define-advice calendar-set-mode-line
      (:override (description) koek-ml/set-mode-line)
    (setq mode-line-format
          `(,@koek-ml/dummies " "
            koek-ml/eldoc koek-ml/ace koek-ml/depth
            koek-ml/exwm-workspaces koek-ml/eyebrowse
            koek-ml/id koek-ml/keycast
            ("" koek-ml/large-separator ,description)
            koek-ml/task koek-ml/modes))
    (force-mode-line-update))

  (define-advice calendar-update-mode-line
      (:override () koek-ml/update-mode-line)
    ;; Calendar buffer isn't guaranteed to exist or be current
    (when-let ((buffer (get-buffer calendar-buffer)))
      (with-current-buffer buffer
        (calendar-set-mode-line
         (calendar-date-string (calendar-current-date)
                               'abbreviate 'nodayname)))))
  :config
  (setq x-underline-at-descent-line t)
  (setq moody-mode-line-height 24)
  (setq-default mode-line-format
                `(,@koek-ml/dummies " "
                  koek-ml/eldoc koek-ml/ace koek-ml/ediff
                  koek-ml/depth koek-ml/exwm-workspaces koek-ml/eyebrowse
                  koek-ml/id koek-ml/state keycast-marker
                  koek-ml/position koek-ml/pdf koek-ml/exwm-input koek-ml/input
                  koek-ml/flymake koek-ml/vc koek-ml/task koek-ml/modes)))

(blink-cursor-mode 0)

(use-package calendar
  :defer t
  :config
  ;; Prefer calling calendar-set-date-style to setting
  ;; calendar-date-style. Calling calendar-set-date-style sets related
  ;; variables.
  (calendar-set-date-style 'european)
  (setq calendar-week-start-day 1))     ; Monday

;; Dependency of org-caldav
(use-package ox-icalendar
  :defer t
  :config
  (setq org-icalendar-timezone "Europe/Brussels"))

(use-package auth-source
  :defer t
  :config
  (setq auth-sources '("secrets:Login")))

(defun koek/get-user-dir (name)
  "Return user directory NAME."
  (when-let ((file-name (getenv name)))
    (file-name-as-directory file-name)))

(defconst koek/home-dir (expand-file-name "~/")
  "File name to home directory.")

(defconst koek/documents-dir
  (or (koek/get-user-dir "XDG_DOCUMENTS_DIR") koek/home-dir)
  "File name to documents directory.")

(defconst koek/download-dir
  (or (koek/get-user-dir "XDG_DOWNLOAD_DIR") koek/home-dir)
  "File name to download directory.")

(defconst koek/music-dir
  (or (koek/get-user-dir "XDG_MUSIC_DIR") koek/home-dir)
  "File name to music directory.")

(defconst koek/calendars-dir
  (or (koek/get-user-dir "KOEK_CALENDARS_DIR") koek/documents-dir)
  "File name to calendars directory.")

(defconst koek/contacts-dir
  (or (koek/get-user-dir "KOEK_CONTACTS_DIR") koek/documents-dir)
  "File name to contacts directory.")

(defconst koek/news-dir
  (or (koek/get-user-dir "KOEK_NEWS_DIR") koek/documents-dir)
  "File name to news directory.")

(defconst koek/notes-dir
  (or (koek/get-user-dir "KOEK_NOTES_DIR") koek/documents-dir)
  "File name to notes directory.")

(defconst koek/projects-dir
  (or (koek/get-user-dir "KOEK_PROJECTS_DIR") koek/home-dir)
  "File name to projects directory.")

(use-package exar
  :koek t
  :after exwm
  :config
  (let ((icc-dir (expand-file-name "icc/" (xdg-data-home))))
    (setq exar-monitors
          `(:laptop (:edid "0x06af3d13000000002617" :name "Laptop"
                     :color ,(expand-file-name "laptop.icc" icc-dir))
            :home (:edid "0x35491800000000000013" :name "Home"
                   :color ,(expand-file-name "home.icc" icc-dir))))
    (setq exar-layouts
          '((:name "Home (primary) and laptop"
             :monitors
             (:home (:top 0 :left 1920 :width 1920 :height 1080)
              :laptop (:top 0 :left 0 :width 1920 :height 1080 :workspaces (1 6))))
            (:name "Home"
             :monitors (:home (:top 0 :left 0 :width 1920 :height 1080)))
            (:name "Laptop"
             :monitors (:laptop (:top 0 :left 0 :width 1920 :height 1080))))))
  (exar-enable)
  (exwm-enable))

(use-package org
  :defer t
  :preface
  (defun koek/lock-file-p (file-name)
    "Return whether FILE-NAME is a lock file."
    (string-prefix-p ".#" (file-name-nondirectory file-name)))

  (defun koek/get-subdirs (file-name &optional full)
    "Return subdirectories in directory FILE-NAME.
When optional FULL is truthy, return absolute file names."
    (thread-last (directory-files-and-attributes file-name full)
      (seq-filter (pcase-lambda (`(,file-name ,type))
                    (let ((name (file-name-nondirectory file-name)))
                      (and (eq type t)  ; Directory
                           (not (or (string-equal name ".")
                                    (string-equal name "..")))))))
      (mapcar (lambda (spec)
                (file-name-as-directory (car spec))))))

  (defun koek/get-agenda-dirs ()
    "Return directories storing agenda files."
    (append (list koek/documents-dir koek/calendars-dir)
            (koek/get-subdirs koek/projects-dir 'full)))

  (defun koek/get-agenda-files ()
    "Return agenda files."
    (thread-last (koek/get-agenda-dirs)
      (seq-mapcat (lambda (file-name)
                    (directory-files file-name 'full (rx ".org" line-end))))
      (seq-filter (lambda (file-name)
                    (not (koek/lock-file-p file-name))))
      seq-uniq))

  (define-advice org-agenda-files
      (:before (&rest _args) koek/update-agenda-files)
    (setq org-agenda-files (koek/get-agenda-files)))
  :config
  (setq org-directory koek/documents-dir)
  (setq org-refile-targets
        '((nil . (:maxlevel . 3))       ; Current
          (org-agenda-files . (:maxlevel . 3)))))

(koek-pkg/register belgian-holidays)    ; Optional dependency

(use-package holidays
  :defer t
  :config
  (require 'belgian-holidays)

  (setq calendar-holidays
        (append holiday-belgian-holidays holiday-solar-holidays)))

(use-package org-caldav
  :defer t
  :config
  ;; Local copy
  ;; Store sync state with calendars, simplifying backup
  (setq org-caldav-save-directory koek/calendars-dir)
  (setq org-caldav-inbox (expand-file-name "Afspraken.org" koek/calendars-dir))
  (setq org-caldav-files nil)

  ;; Remote copy
  (setq org-caldav-url
        "https://caldav.fastmail.com/dav/calendars/user/nicolas@dejaeghe.re")
  (setq org-caldav-calendar-id "260b63a1-58cd-4cf2-bf6a-a2e5acfcf995"))

(use-package org-roam
  :after org
  :config
  (setq org-roam-directory koek/notes-dir)
  (setq org-roam-index-file (expand-file-name "index.org" koek/notes-dir))
  (find-file-noselect org-roam-index-file 'nowarn)
  (org-roam-mode))

(use-package org-capture
  :defer t
  :config
  (setq org-capture-templates
        `(("t" "Task" entry (file+olp "Inbox.org" "Taken")
           "* TODO %?"
           :empty-lines 1)
          ("r" "Reply" entry (file+olp "Inbox.org" "Taken")
           ,(string-join '("* TODO Beantwoord %:subject"
                           "DEADLINE: %^t"
                           ""
                           "%a"
                           ""
                           "%?")
                         "\n")
           :empty-lines 1)
          ("a" "Appointment" entry (file "Kalenders/Afspraken.org")
           ,(string-join '("* %^{Name}"
                           "%^T"
                           "%^{LOCATION}p" ; Inserted after heading
                           "%?")
                         "\n")
           :empty-lines 1)
          ("b" "Bookmark" entry (file+olp "Inbox.org" "Bladwijzers")
           ,(string-join '("* %:description %^g"
                           ":PROPERTIES:"
                           ":URL: %L"
                           ":END:"
                           ""
                           "%?")
                         "\n")
           :empty-lines 1)
          ("B" "Book" item (file+olp "Inbox.org" "Media tips" "Boeken")
           "- [[%:link][%(replace-regexp-in-string (rx (one-or-more \"\\n\")) \" \" \"%i\")]]%?")))
  (setq org-capture-templates-contexts '(("r" ((in-mode . "mu4e-view-mode"))))))

(use-package org-roam-capture
  :defer t
  :config
  (setq org-roam-capture-templates
        `(("n" "Note" plain (function org-roam-capture--get-point)
           "%?"
           :file-name "%<%s>-${slug}"
           :head
           ,(string-join '("# -*- ispell-local-dictionary: \"nl_BE\"; -*-"
                           ""
                           "#+TITLE: ${title}"
                           ""
                           "")
                         "\n")
           :unnarrowed t)))
  (setq org-roam-capture-immediate-template
        (append (seq-find (pcase-lambda (`(,key))
                            (string-equal key "n"))
                          org-roam-capture-templates)
                '(:immediate-finish t)))
  (setq org-roam-capture-ref-templates
        `(("w" "Webpage" plain (function org-roam-capture--get-point)
           "%?"
           :file-name "%(secure-hash 'md5 \"${ref}\")" ; One note per reference
           :head
           ,(string-join '("# -*- ispell-local-dictionary: \"nl_BE\"; -*-"
                           ""
                           "#+TITLE: ${title}"
                           "#+ROAM_KEY: ${ref}"
                           ""
                           "")
                         "\n")
           :unnarrowed t))))

(use-package eww
  :defer t
  :config
  (setq eww-download-directory koek/download-dir))

(use-package mu4e-context
  :defer t
  :config
  (setq mu4e-contexts
        (list (make-mu4e-context
               :name "Personal"
               :match-func
               (lambda (message)
                 (when message
                   (string-prefix-p "/Personal/"
                                    (mu4e-message-field message :maildir))))
               :vars
               `((smtpmail-smtp-server   . "smtp.fastmail.com")
                 (smtpmail-smtp-service  . 465)
                 (smtpmail-stream-type   . ssl)
                 ;; refile-folder, drafts-folder, sent-folder and
                 ;; trash-folder are directory file names, i.e., no
                 ;; trailing /
                 (mu4e-refile-folder     . "/Personal/Archive")
                 (mu4e-drafts-folder     . "/Personal/Drafts")
                 (mu4e-sent-folder       . "/Personal/Sent")
                 (mu4e-trash-folder      . "/Personal/Trash")
                 (mu4e-compose-signature . ,user-full-name))))))

(use-package mu4e-vars
  :defer t
  :config
  (setq mu4e-bookmarks
        '((:name "Personal INBOX"
           :query "maildir:/Personal/INBOX"
           :key ?p)
          (:name "Applied Artificial Intelligence"
           :query "maildir:/Personal/Opleidingen/AAI"
           :key ?a)
          (:name "Unread"
           :query "flag:unread"
           :key ?u))))

(use-package bbdb-vcard
  :defer t
  :config
  (setq bbdb-vcard-default-dir koek/contacts-dir))

(use-package elfeed
  :defer t
  :preface
  (defvar koek/feeds
    '(("3Blue1Brown" youtube "UCYO_jab_esuFRV4b17AJtAw" mathematics)
      ("Arch Linux" "https://www.archlinux.org/feeds/news/" notice linux)
      ("Baggers" youtube "UCMV8p6Lb-bd6UZtTc_QD4zA" lisp)
      ("Caches to Caches" "http://cachestocaches.com/feed" blog ai)
      ("Clickspring" youtube "UCworsKCR-Sx6R6-BnIjS2MA" metalwork)
      ("Clojure" reddit month "Clojure" clojure)
      ("ClojureTV" youtube "UCaLlzGqiPE2QRj6sSOawJRg" clojure)
      ("Emacs" reddit "emacs" emacs)
      ("Erlang Solutions" youtube "UCKrD_GYN3iDpG_uMmADPzJQ" erlang)
      ("Erlang" reddit month "erlang" erlang)
      ("Factorio" "https://www.factorio.com/blog/rss" blog program)
      ("Furniture Making" reddit month "FurnitureMaking" woodwork)
      ("Ishitani Furniture" youtube "UC7FkqjV8SU5I8FCHXQSQe9Q" woodwork)
      ("John Heisz" youtube "UCjA8vRlL1c7BDixQRJ39-LQ" woodwork)
      ("Layout Land" youtube "UC7TizprGknbDalbHplROtag" css)
      ("Linux" reddit month "linux" linux)
      ("Lisp" reddit month "lisp" lisp)
      ("LiveOverflow" youtube "UClcE-kVhqyiHCcjYwcpfj9w" hack)
      ("Luke Smith" youtube "UC2eYFnH61tmytImy1mTYvhA" foss)
      ("Mastering Emacs" "https://www.masteringemacs.org/feed" blog emacs)
      ("Mr. Chickadee" youtube "UCHkYrJ2Fbe7pBjEZvkFzi3A" woodwork)
      ("Netflix TechBlog" "https://medium.com/feed/netflix-techblog" blog ai)
      ("Professor Leonard" youtube "UCoHhuummRZaIVX7bD4t2czg" mathematics)
      ("Programming" reddit month "programming" program)
      ("Protesilaos Stavrou" youtube "UC0uTPqBCFIpZxlz_Lv1tk_g" emacs)
      ("ROBOHEMIAN!" youtube "UCPelotG5UTbWYKrMfG0ynKg" electronics)
      ("Rainfall Projects" youtube "UCPO4D4-UeeFQceK8XrgwXug" woodwork metalwork)
      ("Sacha Chua" "http://sachachua.com/blog/category/emacs/feed/" blog emacs)
      ("Two-Bit History" "https://twobithistory.org/feed.xml" blog history)
      ("Wait But Why" "https://waitbutwhy.com/feed" blog popsci)
      ("frank howarth" youtube "UC3_VCOJMaivgcGqPCTePLBA" woodwork)
      ("krtwood" youtube "UCrI3NWmFF45LwKwk5TEYihQ" woodwork)
      ("scanlime" youtube "UCaEgw3321ct_PE4PJvdhXEQ" electronics))
    "List of news feeds.
A news feed is a list of form:

'(\"title\" \"url\" tag-1 tag-2 tag-n) or
'(\"title\" reddit \"id\" tag-1 tag-2 tag-n) or
'(\"title\" reddit month \"id\" tag-1 tag-2 tag-n) or
'(\"title\" youtube \"id\" tag-1 tag-2 tag-n)

Reddit news feeds are tagged with post and YouTube news feeds are
tagged with video.")

  (defun koek/add-feed (title url &rest tags)
    "Add news feed to database.
TITLE and URL are strings.  TAGS are zero or more symbols."
    (setf (elfeed-feed-title (elfeed-db-get-feed url)) title)
    (push (cons url tags) elfeed-feeds))
  :config
  (setq elfeed-db-directory koek/news-dir)

  (use-package recentf
    :defer t
    :config
    (push (rx line-start (literal elfeed-db-directory)) recentf-exclude))

  (dolist (feed koek/feeds)
    (let ((args
           (pcase feed
             (`(,title reddit month ,id . ,tags)
              `(,(concat title " (Reddit - Month)")
                ,(format "https://www.reddit.com/r/%s/top.rss?t=month" id)
                post ,@tags))
             (`(,title reddit ,id . ,tags)
              `(,(concat title " (Reddit)")
                ,(format "https://www.reddit.com/r/%s/top.rss?t=week" id)
                post ,@tags))
             (`(,title youtube ,id . ,tags)
              `(,(concat title " (YouTube)")
                ,(format "https://www.youtube.com/feeds/videos.xml?channel_id=%s" id)
                video ,@tags))
             (_feed
              feed))))
      (apply #'koek/add-feed args))))

(use-package bongo
  :defer t
  :config
  (setq bongo-default-directory koek/music-dir))

(defun koek/get-initial-buffer ()
  "Return initial buffer."
  (let ((layout (current-window-configuration)))
    (org-agenda-list)
    (set-window-configuration layout))
  org-agenda-buffer)

(setq initial-buffer-choice #'koek/get-initial-buffer)

;;; init.el ends here
