;;; prepcast-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:


;;;### (autoloads nil "prepcast" "prepcast.el" (0 0 0 0))
;;; Generated autoloads from prepcast.el

(defvar prepcast-mode nil "\
Non-nil if Prepcast mode is enabled.
See the `prepcast-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `prepcast-mode'.")

(custom-autoload 'prepcast-mode "prepcast" nil)

(autoload 'prepcast-mode "prepcast" "\
Prepare for screencasting.

This is a minor mode.  If called interactively, toggle the
`Prepcast mode' mode.  If the prefix argument is positive, enable
the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='prepcast-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "prepcast" '("prepcast-"))

;;;***

(provide 'prepcast-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; prepcast-autoloads.el ends here
