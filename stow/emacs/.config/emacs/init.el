;;; init.el --- Nicolas' Emacs configuration -*- lexical-binding: t; buffer-read-only: t; -*-

;;; Commentary:

;; Nicolas' Emacs configuration.

;;; Code:

(require 'subr-x)
(require 'seq)

(require 'nsm)

(setq nsm-settings-file
      (expand-file-name "var/nsm-settings.el" user-emacs-directory))
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

(straight-use-package 'no-littering)
(require 'no-littering)

(straight-use-package 'use-package)
(straight-use-package 'delight)         ; Optional dependency
(eval-when-compile
  (require 'use-package))
(require 'bind-key)

(straight-use-package 'org-plus-contrib)

(straight-use-package '(org :type built-in))

(use-package dired
  :bind
  ("C-c f f" . dired)
  :hook (dired-mode . dired-hide-details-mode)
  :config
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (let* ((safe "-lah") ; For safe switches, see `ls-lisp--insert-directory'
         (unsafe (concat safe " --group-directories-first")))
    (setq dired-listing-switches
          (or (and (executable-find "ls") unsafe) safe))))

(use-package dired-aux
  :after dired
  :config
  (setq dired-vc-rename-file t)
  (setq dired-dwim-target #'dired-dwim-target-recent)
  (setq dired-create-destination-dirs 'ask))

(use-package dired-x
  :after dired
  :bind
  ("C-c f C-f" . dired-jump))

(use-package diredfl
  :straight t
  :after dired
  :config
  (diredfl-global-mode))

(setq delete-by-moving-to-trash t)

(use-package projectile
  :straight t
  :demand t
  :bind
  (:map projectile-command-map
   ("x t" . projectile-run-vterm))
  :preface
  (defun koek-proj/forgetp (file-name)
    "Return whether project with root FILE-NAME should be forgotten."
    (not (recentf-include-p file-name)))
  :config
  (define-key projectile-mode-map (kbd "C-c p") projectile-command-map)

  (setq projectile-ignored-project-function #'koek-proj/forgetp)
  (setq projectile-completion-system 'ivy)
  (projectile-mode)
  :delight)

(use-package vc-hooks
  :defer t
  :config
  (setq vc-follow-symlinks t))

(use-package magit-status
  :straight magit
  :bind
  ("C-c f g" . magit-status))

(use-package magit-log
  :bind
  ("C-c f h" . magit-log-buffer-file))

(use-package git-commit
  :defer t
  :preface
  (defun koek-git/check-spelling (force)
    "Check spelling of commit message.
When FORCE is truthy, continue commit unconditionally."
    (let* ((ispell-skip-region-alist    ; Dynamic variable
            (cons (list (rx line-start "#") #'forward-line) ; Comment
                  ispell-skip-region-alist))
           (tick (buffer-chars-modified-tick))
           (result (ispell-buffer)))
      (cond
       (force t)
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
  (("C-c f d" . ediff-files)
   ("C-c f b" . ediff-current-file)))

(use-package ediff-init
  :defer t
  :preface
  (defun koek-diff/unfold-outline ()
    "Unfold outline in outline and derived modes."
    (when (derived-mode-p 'outline-mode) ; org is derived from outline
      (outline-show-all)))
  :config
  ;; Restore window layout on quit
  (let (last-layout)
    ;; Store window layout before it's modified. Control buffer
    ;; doesn't exist yet.
    (add-hook 'ediff-before-setup-hook
              (lambda ()
                (setq last-layout (current-window-configuration))))
    ;; Store window layout in a local hook of control buffer. Current
    ;; is control buffer.
    (add-hook 'ediff-mode-hook
              (lambda ()
                (let ((layout last-layout))
                  ;; Restore window layout
                  (add-hook 'ediff-quit-hook
                            (lambda ()
                              (set-window-configuration layout))
                            'append 'local)))))

  (add-hook 'ediff-prepare-buffer-hook #'koek-diff/unfold-outline))

(use-package ediff-wind
  :defer t
  :config
  (setq ediff-grab-mouse nil)
  (setq ediff-window-setup-function #'ediff-setup-windows-plain)
  (setq ediff-split-window-function #'split-window-right))

(use-package autorevert
  :config
  (global-auto-revert-mode)
  :delight auto-revert-mode)

(use-package recentf
  :config
  (require 'find-func)
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

(use-package swiper
  :straight t
  :bind
  ;; Why does [remap isearch-forward] prevent pdf-view-mode from
  ;; rebinding C-s?
  (("C-s" . swiper-isearch)
   :map swiper-map
   ("C-c j" . swiper-avy)))

(use-package avy
  :straight t
  :bind
  (("C-c j j" . avy-goto-char-timer)
   ([remap goto-line] . avy-goto-line))
  :config
  (setq avy-all-windows nil)
  (setq avy-all-windows-alt 'all-frames)
  (setq avy-keys '(?q ?s ?d ?f ?j ?k ?l ?m))
  (setq avy-dispatch-alist nil))

(use-package link-hint
  :straight t
  :bind
  ("C-c j l" . link-hint-open-link)
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

(defvar koek-af/excluded-modes '(snippet-mode)
  "List of major mode symbols, see `koek-af/maybe-enable'.")

(defun koek-af/maybe-enable ()
  "Enable `auto-fill-mode' conditionally.
Unless current major mode is member of `koek-af/excluded-modes',
enable `auto-fill-mode'."
  (unless (memq major-mode koek-af/excluded-modes)
    (auto-fill-mode)))

(add-hook 'text-mode-hook #'koek-af/maybe-enable)
(delight 'auto-fill-function nil 'emacs)

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
MODE is a major mode symbol.  OPEN-DELIMITERS are one or more
strings."
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

  (defun
      koek-sp/setup-format-c-block-on-return-handler
      (mode &rest open-delimiters)
    "Setup format-c-block handler in MODE for OPEN-DELIMITERS.
MODE is a major mode symbol.  OPEN-DELIMITERS are one or more
strings."
    (dolist (delimiter open-delimiters)
      (sp-local-pair mode delimiter nil
                     ;; For event names, see `single-key-description'
                     :post-handlers '(:add (koek-sp/format-c-block "RET")))))
  :init
  (bind-keys
   ("C-M-{" . beginning-of-defun)
   ("C-M-}" . end-of-defun)
   ("C-S-w" . append-next-kill))
  :config
  (require 'smartparens-config)

  (setq sp-navigate-interactive-always-progress-point t)
  (setq sp-navigate-reindent-after-up nil)
  (setq sp-highlight-pair-overlay nil)
  (koek-sp/setup-separate-sexp-handler 'clojure-mode "(" "[" "{" "\"")
  (koek-sp/setup-separate-sexp-handler 'lisp-mode "(" "\"")
  (koek-sp/setup-separate-sexp-handler 'emacs-lisp-mode "(" "[" "\"")
  (koek-sp/setup-separate-sexp-handler 'scheme-mode "(" "\"")
  (koek-sp/setup-format-c-block-on-return-handler 'c-mode "{")
  (koek-sp/setup-format-c-block-on-return-handler 'c++-mode "{")
  (koek-sp/setup-format-c-block-on-return-handler 'css-mode "{")
  (koek-sp/setup-format-c-block-on-return-handler 'java-mode "{")
  (koek-sp/setup-format-c-block-on-return-handler 'js2-mode "{" "[")
  (koek-sp/setup-format-c-block-on-return-handler 'json-mode "{" "[")
  (koek-sp/setup-format-c-block-on-return-handler 'scad-mode "{")
  (koek-sp/setup-format-c-block-on-return-handler 'python-mode "{" "[" "(")
  :delight)

(use-package paren-face
  :straight t
  :config
  (global-paren-face-mode))

(use-package expand-region
  :straight t
  :bind
  ("C-S-SPC" . er/expand-region)
  :config
  (setq expand-region-smart-cursor t))

(setq-default indent-tabs-mode nil)

(setq sentence-end-double-space nil)

(setq require-final-newline t)

(defun koek-ws/disable-final-empty-line ()
  "Disable final empty line for current."
  (setq-local require-final-newline nil))

(add-hook 'snippet-mode-hook #'koek-ws/disable-final-empty-line)

(use-package whitespace
  :bind
  ("C-c e c" . whitespace-cleanup)
  :hook ((prog-mode conf-mode) . whitespace-mode)
  :config
  (setq whitespace-action '(auto-cleanup))
  (setq whitespace-style '(face trailing empty lines-tail))
  :delight)

(use-package hl-line
  :hook (prog-mode . hl-line-mode))

(bind-keys
 ("C-c e s" . sort-lines)
 ("C-c e a" . align-regexp))

(bind-keys
 ([remap downcase-word]   . downcase-dwim)
 ([remap upcase-word]     . upcase-dwim)
 ([remap capitalize-word] . capitalize-dwim))

(setq save-interprogram-paste-before-kill t)

(setq tab-always-indent 'complete)

(use-package company
  :straight t
  :bind
  (:map company-mode-map
   ([remap indent-for-tab-command] . company-indent-or-complete-common)
   :map company-active-map
   ("C-n" . company-select-next)
   ("C-p" . company-select-previous))
  :hook ((prog-mode conf-mode comint-mode cider-repl-mode) . company-mode)
  :preface
  (defmacro koek-cpny/setup-backends (&rest args)
    "Setup backends in modes.
ARGS are one or more mode name symbols followed by a list of
backends, see `company-backends'."
    (let ((setup-backends-name (gensym))
          (modes (butlast args))
          (backends (car (last args))))
      `(let ((,setup-backends-name (lambda ()
                                     (setq-local company-backends ',backends))))
         ,@(mapcar (lambda (mode)
                     `(add-hook ',(intern (concat (symbol-name mode) "-hook"))
                                ,setup-backends-name))
                   modes))))

  ;; Prevent geiser from modifying company-backends
  (define-advice geiser-company--setup-company
      (:around (f &rest args) koek-cpny/disable-setup-backends)
    (let ((backends company-backends))
      (apply f args)
      (setq company-backends backends)))
  :config
  (setq company-backends
        '((company-capf company-files :with company-yasnippet)))
  (setq company-idle-delay 1)           ; In seconds
  (setq company-show-numbers t)
  (koek-cpny/setup-backends indium-repl-mode
   ((company-indium-repl company-files :with company-yasnippet)))
  (koek-cpny/setup-backends geiser-mode geiser-repl-mode
   ((geiser-company-backend company-files :with company-yasnippet)))
  (koek-cpny/setup-backends scad-mode
   ((company-dabbrev-code company-files :with company-yasnippet)))
  (koek-cpny/setup-backends conf-mode
   ((company-dabbrev company-files :with company-yasnippet)))
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

(use-package eglot
  :straight t
  :bind
  (:map eglot-mode-map
   ("C-c e f" . eglot-code-actions)
   ("C-c e r" . eglot-rename))
  :hook
  ((c-mode c++-mode erlang-mode mhtml-mode css-mode java-mode js2-mode json-mode python-mode) .
   eglot-ensure)
  :config
  ;; Eclipse JDT Language Server lacks an executable. Eglot expects to
  ;; find the jdtls launcher on the CLASSPATH environment variable.
  (when-let
      ((launcher-program-name
        (thread-last '("/usr/share/java/jdtls/plugins/" "c:/bin/jdtls/plugins/")
          (seq-filter #'file-exists-p)
          (seq-mapcat (lambda (file-name)
                        (directory-files file-name 'full
                                         (rx "org.eclipse.equinox.launcher_"
                                             (one-or-more (or alnum punct))
                                             ".jar" line-end))))
          car)))
    (let ((paths (split-string (or (getenv "CLASSPATH") "") path-separator)))
      (unless (member launcher-program-name paths)
        (setenv "CLASSPATH"
                (string-join (cons launcher-program-name paths)
                             path-separator)))))

  ;; Register additional language servers
  (push '((c-mode c++-mode) . ("clangd")) eglot-server-programs)
  (push '(mhtml-mode . ("html-languageserver" "--stdio")) eglot-server-programs)
  (push '(css-mode . ("css-languageserver" "--stdio")) eglot-server-programs)
  (push '(json-mode . ("json-languageserver" "--stdio")) eglot-server-programs))

(use-package xref
  :straight t
  :defer t
  :config
  (add-to-list 'xref-prompt-for-identifier #'xref-find-references 'append))

(use-package abbrev
  :hook ((sql-mode sql-interactive-mode) . abbrev-mode)
  :config
  ;; Prime abbrev tables
  (unless (file-exists-p (no-littering-expand-var-file-name "abbrev.el"))
    (require 'find-func)

    ;; Upcase SQL keywords
    (let ((abbrevs
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
             (mapcar (lambda (keyword)
                       (list keyword (upcase keyword)))
                     keywords))))
      (define-abbrev-table 'sql-mode-abbrev-table abbrevs)
      (define-abbrev-table 'sql-interactive-mode-abbrev-table abbrevs)))

  (setq save-abbrevs 'silently)
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
Assumes source path is a child of project root."
    (let ((parts
           (let ((separator (thread-first (expand-file-name "a" "b")
                              file-relative-name
                              (substring 1 2)))
                 (rel-file-name
                  (let ((file-name (buffer-file-name)))
                    (if (and (projectile-project-p) file-name)
                        (file-relative-name (file-truename file-name)
                                            (projectile-project-root))
                      (buffer-name)))))
             (let ((file-name (file-name-sans-extension rel-file-name)))
               (or (cdr (split-string file-name (regexp-quote separator)))
                   (list file-name))))))
      (replace-regexp-in-string "_" "-" (string-join parts "."))))

  ;; Org & Markdown
  (defvar koek-ys/lang-specs
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
`koek-ys/lang-specs'."
    (plist-get (seq-find (lambda (spec)
                           (string= (plist-get spec from) lang))
                         koek-ys/lang-specs)
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
                              koek-ys/lang-specs)))

  (defun koek-ys/complete-org ()
    "Complete Org language code from IETF codes."
    (koek-ys/complete-field (mapcar (lambda (spec)
                                      (cons (plist-get spec :ietf)
                                            (plist-get spec :org)))
                                    koek-ys/lang-specs)))

  (defun koek-ys/make-ensure-result-directory (name)
    "Return function to ensure result directory of current code block exists.
NAME is a string, the variable name storing the result file
name."
    (lambda ()
      (when-let ((file-name
                  (file-name-directory
                   (or (koek-org/get-code-block-var-value name) ""))))
        (make-directory file-name 'parents))))
  :config
  ;; Load own snippets
  (setq yas-snippet-dirs (remq 'yas-installed-snippets-dir yas-snippet-dirs))
  (yas-reload-all)

  ;; Set new snippet file snippet
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name "yasnippet/snippets/snippet-mode/new"
                       no-littering-etc-directory))
    (setq yas-new-snippet-default
          (buffer-substring (re-search-forward (rx line-start "# --\n"))
                            (point-max))))
  :delight yas-minor-mode)

(use-package undo-tree
  :straight t
  :demand t
  :bind
  (:map undo-tree-map
   ("M-/" . undo-tree-redo))
  :config
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

(use-package flymake
  :straight t
  :bind
  (:map flymake-mode-map
   ("C-c e n" . flymake-goto-next-error)
   ("C-c e p" . flymake-goto-prev-error)
   ("C-c e l" . flymake-show-diagnostics-buffer))
  :hook (emacs-lisp-mode . flymake-mode)
  :config
  (setq flymake-wrap-around nil)
  :delight)

(use-package flymake-proc
  :defer t
  :config
  (remove-hook 'flymake-diagnostic-functions #'flymake-proc-legacy-flymake))

(setq window-resize-pixelwise t)

(unbind-key "C-z")
(unbind-key "C-x C-z")

(bind-keys
 ("C-c w h" . split-window-below)
 ("C-c w v" . split-window-right)
 ("C-c w b" . balance-windows)
 ("C-c w d" . delete-window)
 ("C-c w C-d" . delete-other-windows)
 ("C-c w M-d" . kill-buffer-and-window))

(defvar koek-wind/n-hor-steps 32
  "Number of horizontal steps to resize a window from monitor width to zero.")

(defun koek-wind/resize (shrink vertical)
  "Resize selected window.
When SHRINK is truthy, shrink window, else, grow window.  When
VERTICAL is truthy, resize vertically, else, resize
horizontally."
  (let* ((width (nth 2 (frame-monitor-geometry)))
         (step (/ width koek-wind/n-hor-steps))
         (delta (if shrink
                    (* step -1)
                  step)))
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
      (setq-default mode-line-format format)))
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
counterclockwise, else, rotate clockwise."
    (interactive "P")
    (if arg
        (rotate-frame-anticlockwise)
      (rotate-frame-clockwise)))
  :init
  (bind-keys
   ("C-c w f" . koek-tf/flip)
   ("C-c w c" . koek-tf/rotate)))

(use-package winner
  :demand t
  :bind
  (("C-c w l" . winner-undo)
   ("C-c w r" . winner-redo))
  :config
  (winner-mode))

(use-package eyebrowse
  :straight t
  :unless (string= (getenv "XDG_CURRENT_DESKTOP") "EXWM")
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

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-trailing-separator-p t))

(use-package ibuffer
  :bind
  ([remap list-buffers] . ibuffer))

(defun koek-buff/bury (&optional arg)
  "Bury current.
With `\\[universal-argument]' prefix argument ARG, kill current."
  (interactive "P")
  (if arg
      (kill-buffer)
    (bury-buffer)))

(bind-key [remap kill-buffer] #'koek-buff/bury)

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
  :when (string= (getenv "XDG_CURRENT_DESKTOP") "EXWM")
  :preface
  ;; For systemctl power management commands, see
  ;; https://access.redhat.com/documentation/en-us/red_hat_enterprise_linux/8/html/configuring_basic_system_settings/managing-services-with-systemd_configuring-basic-system-settings#shutting-down-suspending-hibernating-system_managing-services-with-systemd
  (defun koek-wm/power-off ()
    "Power off system."
    (make-process :name "poweroff" :command '("systemctl" "poweroff")))

  (defun koek-wm/reboot ()
    "Reboot system."
    (make-process :name "reboot" :command '("systemctl" "reboot")))

  (defun koek-wm/kill-power-off (&optional arg)
    "Kill Emacs and power off system.
With `\\[universal-argument]' prefix argument ARG, reboot
system."
    (interactive "P")
    (let ((kill-emacs-hook ; Dynamic variable, restore when kill is aborted
           (append kill-emacs-hook
                   (list (or (and arg #'koek-wm/reboot) #'koek-wm/power-off)))))
      (save-buffers-kill-terminal)))

  (defun koek-wm/suspend ()
    "Suspend system."
    (interactive)
    (make-process :name "suspend" :command '("systemctl" "suspend")))

  (defun koek-wm/launch-firefox (&optional arg)
    "Launch Firefox.
With `\\[universal-argument]' prefix argument ARG, create private
window."
    (interactive "P")
    (make-process
     :name "firefox"
     :command `("firefox" ,(or (and arg "--private-window") "--new-window"))))

  (defun koek-wm/launch-pavc ()
    "Launch PulseAudio Volume Control."
    (interactive)
    (make-process :name "pavc" :command '("pavucontrol")))
  :init
  ;; :init of deferred subpackages are evaluated in both exwm and non
  ;; exwm sessions. :init of guarded (:when) main package is evaluated
  ;; only in exwm sessions. Bind global keys in :init of guarded main
  ;; package.
  (bind-keys
   ("C-c z p" . koek-wm/kill-power-off)
   ("C-c z z" . koek-wm/suspend)
   ("C-c x f" . koek-wm/launch-firefox)
   ("C-c x p" . koek-wm/launch-pavc)
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
   ("C-c w k" . exwm-workspace-delete)))

(use-package exwm-input
  :defer t
  :preface
  (defvar koek-wm/simulation-keys
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
      ("M-d" . "S-C-<right> C-x")
      ("M-DEL" . "S-C-<left> C-x")
      ("C-k" . "S-<end> C-x")
      ("M-@" . "S-C-<right>")
      ("M-h" . "C-a")
      ("M-w" . "C-c")
      ("C-w" . "C-x")
      ("C-y" . "C-v")
      ("C-/" . "C-z")
      ("M-/" . "C-y"))
    "Alist of Emacs keybinding to non Emacs keybinding pairs.
Keybinding is a string, see `edmacro-mode'.")

  (defvar koek-wm/firefox-keys
    (append '(("M-o" . "C-n")
              ("M-p" . "S-C-p")
              ("M-k" . "C-w"))
            koek-wm/simulation-keys)
    "Alist of Emacs keybinding to Firefox keybinding pairs.
Keybinding is a string, see `koek-wm/simulation-keybindings'.")

  (defun koek-wm/setup-application-keys ()
    "Setup application keybindings for current."
    (let* ((class (downcase exwm-class-name))
           (keys (cond
                  ((string-prefix-p "firefox" class)
                   koek-wm/firefox-keys))))
      (when keys
        (exwm-input-set-local-simulation-keys
         (mapcar (pcase-lambda (`(,from . ,to))
                   (cons (kbd from) (kbd to)))
                 keys)))))

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
                '(("s-s" . exwm-input-toggle-keyboard)
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
                  ("s-q" . bury-buffer)
                  ("s-d" . kill-current-buffer)
                  ("<f11>" . exwm-layout-toggle-fullscreen))))

  ;; Translate Emacs to non Emacs keybindings in line mode
  (setq exwm-input-simulation-keys
        (mapcar (pcase-lambda (`(,from . ,to))
                  (cons (kbd from) (kbd to)))
                koek-wm/simulation-keys))

  ;; Grab C-z (repeat) in line mode
  (push ?\C-z exwm-input-prefix-keys)

  (add-hook 'exwm-manage-finish-hook #'koek-wm/setup-application-keys))

(use-package exwm-workspace
  :defer t
  :preface
  (defvar koek-wm/previous-workspace-n nil
    "Previously selected workspace number.")

  (defface koek-wm/selected-workspace '((t :inherit mode-line-emphasis))
    "Face for selected workspace label in mode line."
    :group 'exwm-workspace)

  (defface koek-wm/unselected-workspace nil
    "Face for unselected workspace label in mode line."
    :group 'exwm-workspace)

  (define-advice exwm-workspace-switch
      (:before (&rest _args) koek-wm/update-previous-workspace-n)
    (setq koek-wm/previous-workspace-n exwm-workspace-current-index))

  (defmacro koek-wm/def-switch-workspace (n)
    "Define function to switch to workspace N.
N is an integer, the workspace to switch to."
    `(defun ,(intern (format "koek-wm/switch-workspace-%d" n)) ()
       ,(format "Switch to workspace %d." n)
       (interactive)
       (exwm-workspace-switch-create ,n)))

  (koek-wm/def-switch-workspace 0)
  (koek-wm/def-switch-workspace 1)
  (koek-wm/def-switch-workspace 2)
  (koek-wm/def-switch-workspace 3)
  (koek-wm/def-switch-workspace 4)
  (koek-wm/def-switch-workspace 5)
  (koek-wm/def-switch-workspace 6)
  (koek-wm/def-switch-workspace 7)
  (koek-wm/def-switch-workspace 8)
  (koek-wm/def-switch-workspace 9)

  (defun koek-wm/switch-previous-workspace ()
    "Switch to previously selected workspace."
    (interactive)
    (when koek-wm/previous-workspace-n
      (exwm-workspace-switch-create koek-wm/previous-workspace-n)))

  (defun koek-wm/rename-current ()
    "Rename current according to its class or title."
    ;; Class is the name of an application while instance is the name
    ;; of an instance of the application. For more information, see
    ;; https://www.x.org/releases/X11R7.6/doc/xorg-docs/specs/ICCCM/icccm.html#wm_class_property.
    (let ((class (downcase exwm-class-name)))
      (exwm-workspace-rename-buffer
       (cond
        ((string-prefix-p "gimp" class)
         "GIMP")
        ((string-prefix-p "firefox" class)
         (replace-regexp-in-string
          (rx "Mozilla Firefox" (zero-or-one " (Private Browsing)") line-end)
          "Firefox" (or exwm-title "Firefox")))
        ((string-prefix-p "microsoft teams" class)
         "Teams")
        (t
         exwm-class-name)))))

  (defun koek-wm/n-to-label (n)
    "Convert workspace number N to a workspace label.
N is an integer, a workspace number."
    (or (koek-ml/arabic-to-roman n) "N"))
  :config
  (setq exwm-workspace-number 2)
  (setq exwm-workspace-show-all-buffers t)
  (setq exwm-workspace-index-map #'koek-wm/n-to-label)
  (add-hook 'exwm-update-class-hook #'koek-wm/rename-current)
  (add-hook 'exwm-update-title-hook #'koek-wm/rename-current))

(use-package exwm-layout
  :defer t
  :config
  (setq exwm-layout-show-all-buffers t))

(use-package exwm-floating
  :defer t
  :preface
  (defface koek-wm/floating-border '((t :foreground "white"))
    "Face for border of floating frames."
    :group 'exwm-floating)

  (defun koek-wm/set-floating-border-color ()
    "Set border color of floating frames."
    (setq exwm-floating-border-color
          (face-attribute 'koek-wm/floating-border :foreground)))
  :config
  (add-hook 'koek-thm/enable-hook #'koek-wm/set-floating-border-color))

(use-package server
  :config
  (server-start))

(bind-key "C-z" #'repeat)

(setq enable-recursive-minibuffers t)

(use-package ivy
  :straight t
  :demand t
  :bind
  ("C-r" . ivy-resume)
  :config
  (use-package ivy-avy
    :bind
    (:map ivy-minibuffer-map
     ("C-c j" . ivy-avy)))

  ;; When counsel loads, various commands setup initial input
  (use-package counsel
    :defer t
    :config
    (setq ivy-initial-inputs-alist nil))

  (unbind-key "C-o" ivy-minibuffer-map) ; hydra-ivy/body

  (setq ivy-re-builders-alist
        '((swiper-isearch . ivy--regex-plus)
          (counsel-ag . ivy--regex-plus)
          (counsel-unicode-char . ivy--regex-ignore-order)
          (t . ivy--regex-fuzzy)))
  (setq ivy-use-virtual-buffers t)
  (setq ivy-virtual-abbreviate 'abbreviate)
  (setq ivy-on-del-error-function 'ignore)
  (setq ivy-use-selectable-prompt t)
  (setq ivy-count-format "%d/%d ")
  (ivy-mode)
  :delight)

;; Optional dependencies
(straight-use-package 'ivy-avy)
(straight-use-package 'flx)
(straight-use-package 'wgrep)

(use-package counsel
  :straight t
  :bind
  (([remap find-file] . counsel-find-file)
   ([remap insert-char] . counsel-unicode-char)
   ([remap yank-pop] . counsel-yank-pop)
   ([remap execute-extended-command] . counsel-M-x)
   ([remap info-lookup-symbol] . counsel-info-lookup-symbol)
   ("C-M-s" . counsel-ag)
   ("C-c f s" . counsel-file-jump)
   ("C-c f l" . counsel-find-library)
   ("C-c j d" . counsel-imenu)
   ("C-c j o" . counsel-org-goto-all)
   ("C-c x x" . counsel-linux-app)
   ("C-c x s" . counsel-search)
   :map minibuffer-local-map
   ("C-r" . counsel-minibuffer-history))
  :config
  (ivy-add-actions 'counsel-M-x
                   `(("h"
                      ,(lambda (candidate)
                         (helpful-function (intern candidate)))
                      "help")))

  (setq counsel-linux-app-format-function
        #'counsel-linux-app-format-function-name-first)
  (setq counsel-yank-pop-separator (concat "\n" (make-string 80 ?―) "\n"))
  (setq counsel-org-goto-all-outline-path-prefix 'buffer-name))

(straight-use-package 'request)         ; Optional dependency

(use-package counsel-projectile
  :straight t
  :after projectile
  :config
  (setq counsel-projectile-key-bindings
        (append counsel-projectile-key-bindings
                `((,(kbd "C-p") . counsel-projectile-switch-project)
                  (,(kbd "p")   . counsel-projectile))))

  (counsel-projectile-mode))

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
  :straight (devdocs-lookup :host github :repo "skeeto/devdocs-lookup")
  :bind
  ("C-c d d" . devdocs-lookup)
  :config
  ;; DevDocs is updated more frequently than devdocs-lookup. Update
  ;; subjects.
  (setq devdocs-subjects
        '(("C" "c")
          ("C++" "cpp")
          ("OpenJDK" "openjdk~8")
          ("Clojure" "clojure~1.10")
          ("Erlang" "erlang~21")
          ("HTML" "html")
          ("CSS" "css")
          ("JavaScript" "javascript")
          ("DOM" "dom")
          ("DOM Events" "dom_events")
          ("jQuery" "jquery")
          ("lodash" "lodash~4")
          ("Node.js" "node")
          ("npm" "npm")
          ("Express" "express")
          ("Octave" "octave")
          ("Python" "python~3.8")
          ("NumPy" "numpy~1.17")
          ("pandas" "pandas~0.25")
          ("StatsModels" "statsmodels")
          ("scikit-learn" "scikit_learn")
          ("scikit-image" "scikit_image")
          ("TensorFlow" "tensorflow~python")
          ("Matplotlib" "matplotlib~3.1")
          ("PostgreSQL" "postgresql~12")))
  (devdocs-setup))

(use-package eldoc
  :straight t
  :defer t
  :delight)

(use-package which-key
  :straight t
  :config
  (which-key-add-key-based-replacements
    "C-c &" "yasnippet"
    "C-c d" "documentation"
    "C-c e" "editor"
    "C-c f" "files"
    "C-c j" "jump"
    "C-c k" "media"
    "C-c o" "org"
    "C-c p" "projectile"
    "C-c w" "windows"
    "C-c x" "other"
    "C-c z" "system")
  (which-key-mode)
  :delight)

(use-package vterm
  :straight t
  :commands vterm-mode
  :bind
  (:map vterm-mode-map
   ("C-c C-z" . vterm-send-C-z))
  :preface
  (defvar koek-term/buffer-base-name "*vterm*"
    "Base name of vterm buffers.")

  (defun koek-term/launch (&optional arg)
    "Launch a vterm session or switch to it when one exists.
With numeric prefix argument ARG, launch a numbered session or
switch to it when it exists.  With `\\[universal-argument]'
prefix argument ARG, launch a new numbered session, taking the
first available number."
    (interactive "P")
    (pop-to-buffer-same-window
     (cond
      ((integerp arg)
       (format "%s<%d>" koek-term/buffer-base-name arg))
      (arg
       (generate-new-buffer-name koek-term/buffer-base-name))
      (t
       koek-term/buffer-base-name)))
    (unless (derived-mode-p 'vterm-mode)
      (vterm-mode)))
  :init
  (bind-key "C-c x t" #'koek-term/launch)
  :config
  ;; Resolve keybinding conflict with repeat
  (unbind-key "C-z" vterm-mode-map)

  (setq vterm-kill-buffer-on-exit t))

(use-package vterm-module-make
  :defer t
  :init
  (setq vterm-module-cmake-args "-DUSE_SYSTEM_LIBVTERM=yes"))

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
  (defvar koek-eww/redirect-fs
    (list (lambda (url)
            (require 'url)
            (let ((u (url-generic-parse-url (eww--dwim-expand-url url))))
              (when (string-match-p
                     (rx line-start (zero-or-one "www.") "reddit.com" line-end)
                     (or (url-host u) ""))
                (setf (url-host u) "old.reddit.com")
                (url-recreate-url u)))))
    "List of redirect functions.
Functions are passed a URL and should return a redirect URL or
nil.")

  (define-advice eww (:filter-args (args) koek-eww/redirect)
    (let ((url (car args)))
      (cons (or (seq-find #'identity
                          (mapcar (lambda (f)
                                    (funcall f url))
                                  koek-eww/redirect-fs))
                url)
            (cdr args))))
  :config
  (use-package link-hint
    :bind
    (:map eww-mode-map
     ("j" . link-hint-open-link))))

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
  (let ((config
         (expand-file-name "isync/mbsyncrc"
                           (or (getenv "XDG_CONFIG_HOME") "~/.config/"))))
    (setq mu4e-get-mail-command (format "mbsync -c %s -a" config)))
  (setq mu4e-update-interval (* 60 30)) ; In seconds
  ;; When moving e-mail, isync expects them to be renamed
  (setq mu4e-change-filenames-when-moving t)

  (setq mu4e-completing-read-function #'completing-read)
  (setq mu4e-context-policy 'pick-first)

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
                              (:subject    . nil))))

(use-package mu4e-mark
  :defer t
  :config
  ;; When trashing e-mail, e-mail is flagged trashed. E-mail flagged
  ;; trashed is deleted by most e-mail providers. Move to trash but
  ;; don't flag trashed.
  (push
   (cons
    'trash
    (plist-put (alist-get 'trash mu4e-marks)
               :action
               (lambda (docid _msg target)
                 (mu4e~proc-move docid (mu4e~mark-check-target target) "-N"))))
   mu4e-marks))

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
        (error "%s" "Send aborted"))))
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
                (with-current-buffer
                    (generate-new-buffer
                     (format "*eww: %s*" (elfeed-entry-title entry)))
                  (eww-mode)
                  (eww (elfeed-entry-link entry))
                  (current-buffer))))
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
      (bongo-enqueue-region (or (and next 'insert) 'append)
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

(use-package pdf-view
  :straight pdf-tools
  :mode ((rx ".pdf" string-end) . pdf-view-mode)
  :preface
  (defface koek-pdf/midnight '((t :foreground "white" :background "black"))
    "Face for page when `pdf-view-midnight-minor-mode' is enabled."
    :group 'pdf-view)

  (define-advice pdf-view-midnight-minor-mode
      (:before (&rest _args) koek-pdf/update-colors)
    (setq pdf-view-midnight-colors
          (cons (face-attribute 'koek-pdf/midnight :foreground)
                (face-attribute 'koek-pdf/midnight :background))))

  (defun koek-pdf/re-apply-midnight-colors ()
    "Re-apply `pdf-view-midnight-minor-mode' colors in all buffers."
    (save-current-buffer
      (dolist (buffer (buffer-list))
        (set-buffer buffer)
        (when (and (derived-mode-p 'pdf-view-mode) pdf-view-midnight-minor-mode)
          (pdf-view-midnight-minor-mode 0)
          (pdf-view-midnight-minor-mode)))))
  :config
  ;; Only isearch is supported
  (use-package isearch
    :bind
    (:map pdf-view-mode-map
     ("C-s" . isearch-forward)))

  (setq-default pdf-view-display-size 'fit-page)
  (add-hook 'koek-thm/enable-hook #'koek-pdf/re-apply-midnight-colors)
  :delight (pdf-view-mode "PDF" :major))

(use-package pdf-links
  :bind
  (:map pdf-links-minor-mode-map
   ([remap link-hint-open-link] . pdf-links-action-perform))
  :config
  (use-package link-hint
    :bind
    (:map pdf-links-minor-mode-map
     ("j" . pdf-links-action-perform))))

(use-package pdf-loader
  :config
  (pdf-loader-install 'no-ask 'no-install-deps))

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
  ("C-c x a" . calendar))

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
      (:override () koek-bngo/get-create-default-library-buffer)
    (require 'dired)
    (dired-noselect bongo-default-directory))

  ;; Disable banner
  (define-advice bongo-default-playlist-buffer
      (:override () koek-bngo/get-create-default-playlist-buffer)
    (with-current-buffer (get-buffer-create bongo-default-playlist-buffer-name)
      (unless (derived-mode-p 'bongo-playlist-mode)
        (bongo-playlist-mode))
      (current-buffer)))

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
      (bongo-enqueue-region (or (and next 'insert) 'append)
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
  (bind-keys
   ("C-c k k" . koek-bngo/play-pause)
   :map bongo-dired-library-mode-map
   ([remap bongo-dired-append-enqueue-lines] . koek-bngo/dired-enqueue-dwim)
   ([remap bongo-dired-insert-enqueue-lines] . koek-bngo/dired-enqueue-next-dwim))

  ;; General
  (setq bongo-enabled-backends '(vlc))
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

(use-package keycast
  :straight t
  :defer t
  :config
  (setq keycast-window-predicate #'moody-window-active-p)
  (setq keycast-insert-after 'keycast-marker)
  (setq keycast-separator-width 3)
  (setq keycast-remove-tail-elements nil))

(use-package gino
  :load-path "lisp/gino"
  :commands gino-generate-project)

(use-package cc-mode
  :mode
  (((rx ".c" string-end) . c-mode)
   ((rx ".cpp" string-end) . c++-mode)
   ((rx ".java" string-end) . java-mode))
  :config
  (use-package devdocs-lookup
    :bind
    (:map c-mode-map
     ("C-c d d" . devdocs-lookup-c)
     :map c++-mode-map
     ("C-c d d" . devdocs-lookup-cpp)
     :map java-mode-map
     ("C-c d d" . devdocs-lookup-openjdk~8))))

(use-package cc-cmds
  :defer t
  :preface
  (define-advice c-update-modeline
      (:around (f) koek-cc/disable-update-mode-name)
    (let ((name mode-name))
      (funcall f)
      (setq mode-name name))))

(use-package cc-vars
  :defer t
  :config
  (setq c-default-style
        '((awk-mode  . "awk")
          (java-mode . "java")
          (other     . "stroustrup"))))

(use-package clojure-mode
  :straight t
  :mode
  (((rx ".clj" string-end) . clojure-mode)
   ((rx ".cljs" string-end) . clojurescript-mode)
   ((rx ".cljc" string-end) . clojurec-mode)
   ((rx ".edn" string-end) . clojure-mode))
  :config
  (use-package devdocs-lookup
    :bind
    (:map clojure-mode-map
     ("C-c d d" . devdocs-lookup-clojure~1.10)))
  :delight
  (clojure-mode "Clj" :major)
  (clojurescript-mode "Cljs" :major)
  (clojurec-mode "Cljc" :major))

(use-package cider
  :straight t
  :after clojure-mode)

(use-package cider-mode
  :defer t
  :delight)

(use-package cider-common
  :defer t
  :config
  (setq cider-prompt-for-symbol nil))

(use-package cider-repl
  :defer t
  :config
  ;; Resolve keybinding conflict with company
  (unbind-key "TAB" cider-repl-mode-map)

  (setq cider-repl-use-pretty-printing t))

(use-package lisp-mode
  :mode (rx ".lisp" string-end))

(use-package inf-lisp
  :after lisp-mode
  :config
  (setq inferior-lisp-program "sbcl"))

(use-package sly
  :straight t
  :after lisp-mode)

(use-package sly-mrepl
  :defer t
  :config
  ;; Resolve keybinding conflict with company
  (unbind-key "TAB" sly-mrepl-mode-map)

  (setq sly-mrepl-history-file-name
        (no-littering-expand-var-file-name "sly-mrepl-history")))

(use-package text-mode
  :mode (rx (or ".txt" "/README" "/LICENSE") string-end)
  :delight (text-mode "Txt" :major))

(use-package conf-mode
  :mode (rx (or ".desktop" "cross.txt") string-end)
  :preface
  (defvar koek-conf/mode-names
    '((conf-colon-mode . "Conf:")
      (conf-desktop-mode . "Desktop")
      (conf-javaprop-mode . "Properties")
      (conf-ppd-mode . "PPD")
      (conf-space-mode . "Conf·")
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
            (or (and (not (bound-and-true-p inhibit-mode-name-delight))
                     (alist-get major-mode koek-conf/mode-names))
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

(use-package erlang
  :straight t
  :mode ((rx ".erl" string-end) . erlang-mode)
  :config
  (use-package devdocs-lookup
    :bind
    (:map erlang-mode-map
     ("C-c d d" . devdocs-lookup-erlang~21)))

  ;; On Windows, executable-find finds the erlc shim. Shadow
  ;; c:/ProgramData/chocolatey/bin/.
  (when-let
      ((erlc-program-name
        (car                            ; Assume only one version installed
         (file-expand-wildcards "c:/Program Files/erl*/bin/erlc.exe" 'full))))
    (push (file-name-directory erlc-program-name) exec-path))
  (when-let ((erlc-program-name (executable-find "erlc")))
    (setq erlang-root-dir
          (locate-dominating-file (file-truename erlc-program-name) "bin")))
  :delight (erlang-mode "Erl" :major))

(use-package mhtml-mode
  :mode (rx (or ".htm" ".html") string-end)
  :config
  (use-package devdocs-lookup
    :bind
    (:map mhtml-mode-map
     ("C-c d d" . devdocs-lookup-html)))
  :delight (mhtml-mode "HTML" :major))

(use-package css-mode
  :mode (rx ".css" string-end)
  :config
  (use-package devdocs-lookup
    :bind
    (:map css-mode-map
     ("C-c d d" . devdocs-lookup-css))))

(use-package emmet-mode
  :straight t
  :bind
  (:map emmet-mode-keymap
   ("C-<tab>" . emmet-expand-line))     ; Why does <tab> work but TAB not?
  :hook (mhtml-mode css-mode)
  :config
  (setq emmet-self-closing-tag-style " /")
  :delight)

(use-package js2-mode
  :straight t
  :mode (rx ".js" string-end)
  :interpreter "node"
  :config
  (use-package devdocs-lookup
    :bind
    (:map js2-mode-map
     ("C-c d d" . devdocs-lookup-javascript)
     ("C-c d C-d" . devdocs-lookup-dom)
     ("C-c d C-e" . devdocs-lookup-dom_events)
     ("C-c d C-j" . devdocs-lookup-jquery)
     ("C-c d C-l" . devdocs-lookup-lodash~4)
     ("C-c d C-n" . devdocs-lookup-node)
     ("C-c d C-p" . devdocs-lookup-npm)
     ("C-c d C-x" . devdocs-lookup-express)))

  ;; Resolve keybinding conflict with eglot
  (unbind-key "M-." js2-mode-map)

  (setq js2-mode-show-parse-errors nil)
  (setq js2-mode-show-strict-warnings nil)
  :delight (js2-mode "JS" :major))

;; Prevent indium from creating Chrome profile directory during
;; installation
(setq indium-chrome-data-dir nil)

(use-package indium-interaction
  :straight indium
  :hook (js2-mode . indium-interaction-mode)
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
  (use-package devdocs-lookup
    :bind
    (:map json-mode-map
     ("C-c d C-n" . devdocs-lookup-npm))))

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
  (use-package devdocs-lookup
    :bind
    (:map octave-mode-map
     ("C-c d d" . devdocs-lookup-octave)))

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
  ;; Resolve smartparens' delayed post handlers not being called
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

  (use-package counsel
    :bind
    (:map org-mode-map
     ([remap counsel-imenu] . counsel-org-goto)
     ([remap org-set-tags-command] . counsel-org-tag)))

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
  (setq org-todo-keywords
        '((sequence "TODO(t)" "STALLED(s@/!)" "|" "DONE(d!)" "ABANDONED(a@)")))

  ;; Appearance
  (setq org-adapt-indentation nil)
  (setq org-startup-folded t)
  (setq org-highlight-latex-and-related '(native))
  (push '("" "listings" nil) org-latex-packages-alist)

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
  :delight org-cdlatex-mode)

(use-package org-agenda
  :bind
  ("C-c o a" . org-agenda)
  :config
  (use-package counsel
    :bind
    (:map org-agenda-mode-map
     ([remap org-agenda-set-tags] . counsel-org-tag-agenda)))

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
  :bind
  (:map org-src-mode-map
   ("C-c o '" . org-edit-src-exit)
   ("C-c o k" . org-edit-src-abort))
  :preface
  (define-advice org-src--construct-edit-buffer-name
      (:override (org-buffer-name _lang) koek-org/construct-buffer-name)
    ;; Mirror helpful buffer names
    (format "*org-src: %s*" org-buffer-name))
  :config
  ;; Resolve keybinding conflict with cider
  (unbind-key "C-c '" org-src-mode-map)
  (unbind-key "C-c C-k" org-src-mode-map)

  (push '("js" . js2) org-src-lang-modes)
  :delight)

(use-package ob-core
  :defer t
  :config
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
                 (replace-regexp-in-string (rx (or (and line-start "\"")
                                                   (and "\"" line-end)))
                                           "" (string-join (cdr parts) "="))))))
      (seq-find (pcase-lambda (`(,nm))
                  (string= nm name)))
      cdr))

  ;; Evaluate code blocks in buffer after confirmation
  (let ((whitelist ()))
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
  (add-hook 'org-babel-post-tangle-hook #'koek-org/process-file-end))

(use-package ob-clojure
  :defer t
  :config
  (setq org-babel-clojure-backend 'cider))

(use-package ol
  :bind
  ("C-c o l" . org-store-link)
  :config
  (setq org-link-keep-stored-after-insertion t))

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
         (let ((normalized
                (replace-regexp-in-string (rx (any " \n")) ""
                                          org-latex-hyperref-template)))
           (string-match
            (rx "{" (submatch-n 1 (one-or-more (any letter "={}%,"))) "}")
            normalized)
           (split-string (match-string 1 normalized) ","))))
    (setq org-latex-hyperref-template
          (concat "\\hypersetup{\n "
                  (string-join (append options '("hidelinks")) ",\n ")
                  "\n}\n"))))

(straight-use-package 'htmlize)         ; Optional dependency

(use-package org-roam
  :straight t
  :bind
  ("C-c o f" . org-roam-find-file)
  :preface
  (defun koek-org/title-to-slug (title)
    "Convert note title TITLE to a file name slug.
TITLE is a string, a note title."
    (let ((parts
           (split-string (replace-regexp-in-string (rx (not (any alphanumeric)))
                                                   " " title))))
      (downcase (string-join parts "-"))))
  :config
  (setq org-roam-title-to-slug-function #'koek-org/title-to-slug)
  :delight)

(use-package org-roam-completion
  :defer t
  :config
  (setq org-roam-completion-system 'ivy))

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
  (use-package devdocs-lookup
    :bind
    (:map python-mode-map
     ("C-c d d" . devdocs-lookup-python~3.8)
     ("C-c d C-n" . devdocs-lookup-numpy~1.17)
     ("C-c d C-p" . devdocs-lookup-pandas~0.25)
     ("C-c d C-s" . devdocs-lookup-statsmodels)
     ("C-c d C-l" . devdocs-lookup-scikit_learn)
     ("C-c d C-i" . devdocs-lookup-scikit_image)
     ("C-c d C-t" . devdocs-lookup-tensorflow~python)
     ("C-c d C-m" . devdocs-lookup-matplotlib~3.1)))

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
  (use-package devdocs-lookup
    :bind
    (:map sql-mode-map
     ("C-c d C-p" . devdocs-lookup-postgresql~12)))

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
         ;; Resolve conflict between `file-truename' and
         ;; `query-replace', see
         ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=33697
         (save-match-data
           (let ((file-name (buffer-file-name)))
             (cond
              ((and (projectile-project-p) file-name)
               (format "~%s/%s"
                       (projectile-project-name)
                       (file-relative-name (file-truename file-name)
                                           (projectile-project-root))))
              (file-name
               (abbreviate-file-name file-name))
              (t
               "%b")))))
        " - Emacs"))

(use-package color-theme-sanityinc-tomorrow
  :straight t
  :preface
  (defvar koek-thm/theme-specs
    '((:theme sanityinc-tomorrow-blue     :name "Blue"     :palette blue     :dark t)
      (:theme sanityinc-tomorrow-bright   :name "Bright"   :palette bright   :dark t)
      (:theme sanityinc-tomorrow-day      :name "Day"      :palette day      :dark nil)
      (:theme sanityinc-tomorrow-eighties :name "Eighties" :palette eighties :dark t)
      (:theme sanityinc-tomorrow-night    :name "Night"    :palette night    :dark t))
    "List of theme specifications.
A theme specification is a plist with keys :theme, :name,
:palette and :dark.  :theme is a symbol, a Tomorrow theme
variant.  :name is a string, the theme name.  :palette is a
symbol, the theme color palette, see
`color-theme-sanityinc-tomorrow-colors'.  :dark is a symbol,
whether the theme is dark.")

  (defvar koek-thm/default-theme 'sanityinc-tomorrow-eighties
    "Default theme.")

  (defvar koek-thm/enable-hook nil
    "Normal hook run after enabling theme.")

  (define-advice load-theme (:after (&rest _args) koek-thm/run-enable-hooks)
    (run-hooks 'koek-thm/enable-hook))

  (defun koek-thm/set-frame-theme-variant (frame)
    "Set theme variant of FRAME.
When window theme is dark, theme variant is set to dark, else,
it's set to nothing."
    (when (fboundp 'koek-thm/set-frame-theme-variant-xprop)
      (koek-thm/set-frame-theme-variant-xprop frame)))

  (when (executable-find "xprop")
    (defun koek-thm/set-frame-theme-variant-xprop (frame)
      "Set theme variant of FRAME.
Mustn't be called directly, see
`koek-thm/set-frame-theme-variant'."
      (let* ((spec (seq-find (lambda (spec)
                               (eq (plist-get spec :theme)
                                   (car custom-enabled-themes)))
                             koek-thm/theme-specs))
             (variant (or (and (plist-get spec :dark) "dark") "")))
        (make-process :name "xprop"
                      :command `("xprop"
                                 "-id" ,(frame-parameter frame 'outer-window-id)
                                 "-f" "_GTK_THEME_VARIANT" "8u"
                                 "-set" "_GTK_THEME_VARIANT" ,variant)))))

  (add-hook 'after-make-frame-functions #'koek-thm/set-frame-theme-variant)

  (defun koek-thm/hex-to-rgb (color)
    "Convert hex color COLOR to RGB.
COLOR is a string, a color of format \"#RGB\".  R, G and B are
one or more hexadecimal digits, the red, green and blue color
component."
    (let ((normalized
           (progn
             (string-match (rx (submatch-n 1 (one-or-more (any hex-digit))))
                           color)
             (match-string 1 color))))
      (mapcar (lambda (component)
                (string-to-number component 16))
              (seq-partition normalized (/ (length normalized) 3)))))

  (defun koek-thm/rgb-to-hex (color)
    "Convert RGB color COLOR to hex.
COLOR is a list, a color of format '(R G B).  R, G and B are
floats from zero to 255, the red, green and blue color
component."
    (apply #'format "#%02x%02x%02x" (mapcar #'round color)))

  (defun koek-thm/mix (color1 color2 &optional ratio)
    "Mix COLOR1 with COLOR2.
COLOR1 and COLOR2 are strings, a hex color, see
`koek-thm/hex-to-rgb'.  Optional RATIO is a float from zero to
one and defaults to one half.  Zero means mix zero units of
COLOR1 with one unit of COLOR2, one means mix one unit of COLOR1
with zero units of COLOR2."
    (unless ratio
      (setq ratio 0.5))
    (let ((ratio´ (- 1 ratio)))
      (koek-thm/rgb-to-hex
       (seq-mapn (lambda (component component´)
                   (+ (* component ratio) (* component´ ratio´)))
                 (koek-thm/hex-to-rgb color1) (koek-thm/hex-to-rgb color2)))))

  (defun koek-thm/enable (theme-spec)
    "Enable THEME-SPEC.
THEME-SPEC is a theme specification, see
`koek-thm/tmrw-theme-specs'."
    (interactive
     (list
      (let ((candidates
             (mapcar (lambda (spec)
                       (cons (format "%s (%s)"
                                     (plist-get spec :name)
                                     (or (and (plist-get spec :dark) "dark")
                                         "light"))
                             spec))
                     koek-thm/theme-specs)))
        (cdr (assoc (completing-read "Theme: " candidates nil t) candidates)))))
    (require 'map)

    ;; Set window theme
    (mapc #'disable-theme custom-enabled-themes)
    (let* ((palette
            (let* ((theme (alist-get (plist-get theme-spec :palette)
                                     color-theme-sanityinc-tomorrow-colors))
                   (personal
                    (map-let (foreground background) theme
                      `((alt-bg . ,(koek-thm/mix background foreground 0.98))
                        (lc-fg  . ,(koek-thm/mix foreground background))))))
              (append personal theme)))
           (specs
            (map-let
                (foreground background current-line selection comment red alt-bg lc-fg)
                palette
              `(;; Default
                (show-paren-match    :foreground ,foreground :background ,selection)
                (show-paren-mismatch :foreground ,foreground :background unspecified
                                     :underline (:style wave :color ,red))
                (header-line         :background unspecified)
                (fringe              :background ,alt-bg)
                (mode-line           :background ,current-line
                                     :underline ,selection :overline ,selection
                                     :box unspecified :weight unspecified)
                (mode-line-inactive  :background ,alt-bg
                                     ;; Moody doesn't see inherited
                                     ;; attributes
                                     :underline ,selection :overline ,selection
                                     :weight unspecified)
                ;; avy
                (avy-lead-face   :weight normal :slant normal)
                (avy-lead-face-0 :inherit avy-lead-face)
                (avy-lead-face-1 :inherit avy-lead-face)
                (avy-lead-face-2 :inherit avy-lead-face)
                ;; paren-face
                (parenthesis :foreground ,lc-fg)
                ;; whitespace
                (whitespace-trailing :foreground unspecified :background ,selection)
                (whitespace-empty    :foreground unspecified :background ,selection)
                (whitespace-line     :foreground unspecified :background ,selection)
                ;; ace-window
                (aw-leading-char-face :background ,background
                                      :weight bold :slant normal)
                (aw-background-face   :background ,background)
                (aw-mode-line-face    :inherit aw-leading-char-face)
                ;; eyebrowse
                (eyebrowse-mode-line-active :foreground ,foreground)
                ;; exwm
                (koek-wm/floating-border      :foreground ,selection)
                (koek-wm/selected-workspace   :foreground ,foreground :weight bold)
                (koek-wm/unselected-workspace :foreground ,comment)
                ;; pdf-tools
                (koek-pdf/midnight :foreground ,foreground :background ,background)
                ;; dictionary
                (dictionary-word-entry-face      :weight bold)
                (dictionary-word-definition-face :family unspecified)
                (dictionary-reference-face       :inherit link)))))
      ;; Adding attributes to a face before it's defined, fails. Add
      ;; attributes to user theme.
      (apply #'custom-set-faces
             (mapcar (pcase-lambda (`(,name . ,attrs))
                       `(,name ((t . ,attrs))))
                     specs)))
    (load-theme (plist-get theme-spec :theme) 'no-confirm)

    ;; Set frame theme
    (mapc #'koek-thm/set-frame-theme-variant (frame-list)))
  :config
  (when-let ((spec (seq-find (lambda (spec)
                               (eq (plist-get spec :theme)
                                   koek-thm/default-theme))
                             koek-thm/theme-specs)))
    (koek-thm/enable spec)))

(defvar koek-font/pairs
  '(((:family "PragmataPro Mono" :height 110)
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
                  (seq-every-p (lambda (attrs)
                                 (member (plist-get attrs :family) families))
                               pair))
                koek-font/pairs)))
            (fixed (car pair))
            (variable (cadr pair)))
  (apply #'set-face-attribute 'default nil fixed)
  (apply #'set-face-attribute
         'fixed-pitch nil (plist-put (copy-sequence fixed) :height 1.0))
  (apply #'set-face-attribute 'variable-pitch nil variable))

(blink-cursor-mode 0)

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
    (substring-no-properties (window-parameter nil 'ace-window-path)))

  (defconst koek-ml/ace
    '(:eval
      (when (bound-and-true-p ace-window-mode)
        `(,(moody-ribbon
            (propertize (koek-ml/get-window-label) 'face 'aw-mode-line-face)
            nil 'up)
          koek-ml/separator)))
    "Ace mode line construct.")

  (defvar-local koek-ml/variant nil
    "Ediff variant.")

  (defconst koek-ml/ediff
    '(:eval
      (when koek-ml/variant
        `(,(moody-ribbon
            (concat (plist-get koek-ml/variant :label)
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
                  (member exwm-workspace-current-index ns)))
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
                            (unless (string= name "")
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
          (when (or (> (length workspaces) 1) (not (= selected-n 0)))
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
    (substring s 0 (min length (length s))))

  (defconst koek-ml/id
    '(:eval
      (moody-tab
       (concat (when (and (projectile-project-p) (derived-mode-p 'prog-mode))
                 (concat (koek-ml/truncate (projectile-project-name) 16) "/"))
               (propertize (koek-ml/truncate (buffer-name) 32)
                           'face 'mode-line-buffer-id))))
    "Id mode line construct.")

  (defconst koek-ml/state '(" " "%*%+")
    "State mode line construct.")

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
      (elisp-flymake-checkdoc . "Checkdoc"))
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
    "Return number of diagnosis per type of error."
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
              "- -")
             (`finished
              (let ((n-diags (koek-ml/get-flymake-n-diags)))
                (mapconcat
                 (lambda (cat)
                   (propertize
                    (number-to-string (alist-get cat n-diags 0))
                    'face (flymake--lookup-type-property cat 'mode-line-face)))
                 '(flymake-error flymake-warning) " ")))
             (state
              (koek-ml/state-to-description state))))))
    "Flymake mode line construct.")

  (defconst koek-ml/vc
    '(:eval
      (when (and (bound-and-true-p vc-mode) (moody-window-active-p))
        (let ((state (string-trim (substring-no-properties vc-mode))))
          ;; For format, see `vc-default-mode-line-string'
          (string-match
           (rx (submatch-n 1 (one-or-more (not (any "-:@!?")))) (any "-:@!?")
               (zero-or-one (not (any ":")) ":")
               (submatch-n 2 (one-or-more not-newline)))
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

  (defvar koek-ml/ediff-buffer-specs
    '((:type A   :buffer ediff-buffer-A)
      (:type B   :buffer ediff-buffer-B)
      (:type C   :buffer ediff-buffer-C)
      (:type Anc :buffer ediff-ancestor-buffer))
    "List of ediff buffer specifications.
An ediff buffer specification is a plist with keys :type and
:buffer.  :type is a symbol, a type of buffer.  :buffer is a
symbol, the variable name storing the buffer of type :type.")

  (defun koek-ml/get-variants ()
    "Return variants of ediff session.
Must be called from control buffer."
    (seq-reduce (lambda (variants spec)
                  (let ((buffer (symbol-value (plist-get spec :buffer))))
                    (when (buffer-live-p buffer)
                      (let ((type (plist-get spec :type)))
                        (push (list :type type
                                    :buffer buffer
                                    :label (symbol-name type))
                              variants))))
                  variants)
                koek-ml/ediff-buffer-specs ()))

  (defun koek-ml/get-variant-state (variant)
    "Return state of VARIANT.
Must be called from control buffer."
    (when (ediff-valid-difference-p)
      (let ((state
             (pcase (plist-get variant :type)
               ('C
                (concat
                 (ediff-get-state-of-diff ediff-current-difference 'C)
                 (when-let
                     ((merge
                       (ediff-get-state-of-merge ediff-current-difference)))
                   (concat " " merge))
                 (when (ediff-get-state-of-ancestor ediff-current-difference)
                   " AncEmpty")))
               ('Anc
                (pcase (ediff-get-state-of-merge ediff-current-difference)
                  ("prefer-A"
                   "=diff(B)")
                  ("prefer-B"
                   "=diff(A)")))
               (type
                (ediff-get-state-of-diff ediff-current-difference type)))))
        (unless (string-empty-p state)
          state))))

  (define-advice ediff-refresh-mode-lines
      (:override () koek-ml/update-variants)
    (setq mode-line-format
          `(,@koek-ml/dummies " "
            koek-ml/eldoc koek-ml/ace koek-ml/depth
            koek-ml/exwm-workspaces koek-ml/eyebrowse
            koek-ml/id koek-ml/diff keycast-marker koek-ml/task koek-ml/modes))
    (force-mode-line-update)
    (dolist (variant (koek-ml/get-variants))
      (let ((state (koek-ml/get-variant-state variant)))
        (with-current-buffer (plist-get variant :buffer)
          (setq koek-ml/variant
                (list :label (plist-get variant :label) :state state))
          (force-mode-line-update)))))

  (defun koek-ml/cleanup-variants ()
    "Cleanup variants."
    (save-current-buffer
      (dolist (variant (koek-ml/get-variants))
        (set-buffer (plist-get variant :buffer))
        (kill-local-variable 'koek-ml/variant)
        (force-mode-line-update))))

  (add-hook 'ediff-cleanup-hook #'koek-ml/cleanup-variants)
  :config
  (setq x-underline-at-descent-line t)
  (setq moody-mode-line-height 24)
  (setq-default mode-line-format
                `(,@koek-ml/dummies " "
                  koek-ml/eldoc koek-ml/ace koek-ml/ediff
                  koek-ml/depth koek-ml/exwm-workspaces koek-ml/eyebrowse
                  koek-ml/id koek-ml/state koek-ml/position koek-ml/pdf
                  keycast-marker koek-ml/exwm-input koek-ml/input
                  koek-ml/flymake koek-ml/vc koek-ml/task koek-ml/modes)))

(use-package auth-source
  :defer t
  :config
  (setq auth-sources '("secrets:Login")))

(defun koek/resolve-directory (name)
  "Resolve directory in environment variable NAME."
  (thread-first (or (getenv name) (expand-file-name "~/"))
    file-truename
    file-name-as-directory))

(defconst koek/documents-dir (koek/resolve-directory "XDG_DOCUMENTS_DIR")
  "File name to documents directory.")

(defconst koek/download-dir (koek/resolve-directory "XDG_DOWNLOAD_DIR")
  "File name to download directory.")

(defconst koek/music-dir (koek/resolve-directory "XDG_MUSIC_DIR")
  "File name to music directory.")

(defconst koek/projects-dir (koek/resolve-directory "KOEK_PROJECTS_DIR")
  "File name to projects directory.")

(defconst koek/calendars-dir (koek/resolve-directory "KOEK_CALENDARS_DIR")
  "File name to calendars directory.")

(defconst koek/notes-dir (koek/resolve-directory "KOEK_NOTES_DIR")
  "File name to notes directory.")

(defconst koek/news-dir (koek/resolve-directory "KOEK_NEWS_DIR")
  "File name to news directory.")

(use-package exar
  :load-path "lisp/exar"
  :after exwm
  :config
  (let ((icc-dir
         (expand-file-name "icc/"
                           (or (getenv "XDG_DATA_HOME") "~/.local/share/"))))
    (setq exar-monitors
          `(:laptop (:edid "0x06af3d13000000002617" :name "Laptop"
                     :color ,(expand-file-name "laptop.icc" icc-dir))
            :home (:edid "0x35491800000000000013" :name "Home"
                   :color ,(expand-file-name "home.icc" icc-dir))))
    (setq exar-layouts
          '((:name "Home (primary) and laptop"
             :monitors
             (:home (:top 0 :left 1920 :width 1920 :height 1080)
              :laptop (:top 0 :left 0 :width 1920 :height 1080 :workspaces (1 5))))
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
                           (not (or (string= name ".") (string= name "..")))))))
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
                    (not (koek/lock-file-p file-name))))))

  (define-advice org-agenda-files
      (:before (&rest _args) koek/update-agenda-files)
    (setq org-agenda-files (koek/get-agenda-files)))
  :config
  (setq org-directory koek/documents-dir)
  (setq org-refile-targets
        '((nil . (:maxlevel . 3))       ; Current
          (org-agenda-files . (:maxlevel . 3)))))

(use-package calendar
  :defer t
  :config
  (setq calendar-week-start-day 1))     ; Monday

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

(use-package ox-icalendar
  :defer t
  :config
  (setq org-icalendar-timezone "Europe/Brussels"))

(use-package org-roam
  :after org
  :config
  (setq org-roam-directory koek/notes-dir)
  (setq org-roam-index-file (expand-file-name "index.org" koek/notes-dir))
  (org-roam-mode)
  (find-file-noselect org-roam-index-file 'nowarn))

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
                           "%a"
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
                            (string= key "n"))
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

(use-package mu4e-vars
  :defer t
  :config
  (use-package mu4e-context
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

  (use-package mu4e-utils
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
             :key ?u)))))

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
      ("LEGO" reddit month "lego" lego)
      ("Layout Land" youtube "UC7TizprGknbDalbHplROtag" css)
      ("Linux" reddit month "linux" linux)
      ("Lisp" reddit month "lisp" lisp)
      ("LiveOverflow" youtube "UClcE-kVhqyiHCcjYwcpfj9w" hack)
      ("Luke Smith" youtube "UC2eYFnH61tmytImy1mTYvhA" foss)
      ("Mastering Emacs" "https://www.masteringemacs.org/feed" blog emacs)
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
News feed is a list of format:

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

(defun koek/make-initial-buffer ()
  "Return initial buffer."
  (let ((layout (current-window-configuration)))
    (org-agenda-list)
    (set-window-configuration layout))
  (get-buffer "*Org Agenda*"))

(setq initial-buffer-choice #'koek/make-initial-buffer)

;;; init.el ends here
