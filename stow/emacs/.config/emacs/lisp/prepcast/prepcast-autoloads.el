;;; prepcast-autoloads.el --- automatically extracted autoloads
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

If called interactively, enable Prepcast mode if ARG is positive,
and disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it if
ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "prepcast" '("prepcast-")))

;;;***

(provide 'prepcast-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; prepcast-autoloads.el ends here
