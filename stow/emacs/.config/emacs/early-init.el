(setq gc-cons-threshold (* (expt 1024 2) 128)) ; In bytes

(setq package-enable-at-startup nil)

(push '(fullscreen . maximized) default-frame-alist)

;; Prefer setting frame parameters to disabling menu-bar-mode,
;; tool-bar-mode and scroll-bar-mode during early initialization
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars . nil) default-frame-alist)
