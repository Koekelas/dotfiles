;;; koek-ml-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "koek-ml" "koek-ml.el" (0 0 0 0))
;;; Generated autoloads from koek-ml.el

(defvar koek-ml-mode nil "\
Non-nil if Koek-Ml mode is enabled.
See the `koek-ml-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `koek-ml-mode'.")

(custom-autoload 'koek-ml-mode "koek-ml" nil)

(autoload 'koek-ml-mode "koek-ml" "\
Custom mode line.

If called interactively, enable Koek-Ml mode if ARG is positive,
and disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it if
ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "koek-ml" '("koek-ml/")))

;;;***

(provide 'koek-ml-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; koek-ml-autoloads.el ends here
