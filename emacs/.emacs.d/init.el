;;; init.el --- Koekelas' Emacs configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; Koekelas' Emacs configuration.

;;; Code:

;;; File variables
;; Setting file variables to unsafe values must be confirmed. When
;; this happens during initialization of exwm, Emacs hangs. Mark
;; project's file variables safe.
(push '(counsel-ag-base-command . "ag --nocolor --nogroup --hidden %s")
      safe-local-variable-values)

;;; Garbage collector
;; Increasing cons threshold makes garbage collection more efficient
;; and decreasing it makes garbage collection less noticeable, i.e.,
;; cons threshold is a tradeoff between the runtime of the garbage
;; collector and the responsiveness of Emacs. Increase cons threshold
;; during initialization.
(setq gc-cons-threshold (* (expt 1024 2) 128)) ; In bytes
;; Once initialized, gcmh kicks in

;;; nsm - Network security manager
(require 'nsm)

;; no-littering requires straight which requires nsm. Set location of
;; settings file myself.
(setq nsm-settings-file
      (expand-file-name "var/nsm-settings.el" user-emacs-directory))
(setq network-security-level 'high)

;;; straight - Package manager
(defvar bootstrap-version)              ; Must be a dynamic variable

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

;;; no-littering - Normalize configuration and data paths packages
(straight-use-package 'no-littering)
(require 'no-littering)

;;; use-package - Package configuration macro
(straight-use-package 'use-package)
(straight-use-package 'delight)         ; Optional dependency

;;; org - Notes, to-do lists and project planning
;; Installing latest org after loading builtin org breaks org. Install
;; latest org before loading literate configuration.
(straight-use-package 'org-plus-contrib)
;; org is a subset of org-plus-contrib. Packages depending on org will
;; cause org to be installed even when org-plus-contrib is installed.
;; Prevent packages from installing org.
(straight-use-package '(org :type built-in))
;; org is configured elsewhere

;;; cus-edit - Configuration interface
(setq custom-file (no-littering-expand-var-file-name "custom.el"))
(load custom-file 'no-error)

;;; Literate configuration
;; directory-files returns a sorted list, i.e., 00-*.org is loaded
;; before 10-*.org, 10-*.org before 20-*.org, etc.
(mapc #'org-babel-load-file
      (directory-files user-emacs-directory 'full (rx ".org" line-end)))

;;; init.el ends here
