;;; koek-ml-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
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

This is a minor mode.  If called interactively, toggle the
`Koek-Ml mode' mode.  If the prefix argument is positive, enable
the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='koek-ml-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "koek-ml" '("koek-ml/"))

;;;***

(provide 'koek-ml-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; koek-ml-autoloads.el ends here
