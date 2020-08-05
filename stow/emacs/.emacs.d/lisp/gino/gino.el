;;; gino.el --- Generate Arduino project -*- lexical-binding: t; -*-

;;; Commentary:

;; Generate an Arduino project backed by Meson

;;; Code:

(require 'subr-x)
(require 'seq)

(defvar gino-hardware-install-dirs '("/usr/share/arduino/hardware/")
  "List of file names to hardware specification install directories.")

(defvar gino-hardware-whitelist-preds
  (list (lambda (spec)
          (let ((name (gino--get-in '(platform name) spec))
                (version (gino--get-in '(platform version) spec)))
            (and (string= name "Arch Linux Arduino AVR Boards")
                 (string-match-p
                  (rx line-start "1.8." (one-or-more num) line-end) version)
                 t))))
  "List of hardware specification whitelist predicates.
Predicates are passed a hardware specification.")

(defun gino--get-in (keys alist)
  "Return value for KEYS in nested ALIST.
KEYS is a list of keys."
  (if keys
      (gino--get-in (cdr keys) (assq (car keys) alist))
    (cdr alist)))

(defun gino--set-in (keys value alist)
  "Set value for KEYS to VALUE in nested ALIST.
KEYS is a list of keys."
  (when-let ((key (car keys)))
    (let ((pair (or (assq key alist)
                    (let ((new-pair (cons key ())))
                      (push new-pair alist)
                      new-pair))))
      (setcdr pair (if-let ((rem-keys (cdr keys)))
                       (gino--set-in rem-keys value (cdr pair))
                     value))))
  alist)

(defun gino--extract-keys (compound-key)
  "Extract keys from COMPOUND-KEY.
COMPOUND-KEY is a string of dot separated keys."
  (mapcar #'intern (split-string compound-key (rx "."))))

(defun gino--make-nested-alist (properties)
  "Return nested alist.
PROPERTIES is an alist of compound key to value pairs."
  (seq-reduce (pcase-lambda (alist `(,key . ,value))
                (gino--set-in (gino--extract-keys key) value alist))
              properties ()))

(defun gino--read-properties (file-name)
  "Read properties from FILE-NAME."
  (let ((properties ()))
    (with-temp-buffer
      (insert-file-contents file-name)
      (while (re-search-forward         ; Matches across multiple lines
              (rx line-start
                  (submatch-n 1
                   (not (any "#\n")) (one-or-more (not (any "=")))) "="
                  (submatch-n 2 (one-or-more not-newline)) line-end)
              nil t)
        (push (cons (match-string 1) (match-string 2)) properties)))
    (gino--make-nested-alist properties)))

(defun gino--read-hardware-specs (file-names)
  "Read hardware specifications from FILE-NAMES.
FILE-NAMES is a list of file names to hardware specification
install directories."
  (thread-last file-names
    (seq-mapcat
     (lambda (file-name)
       (file-expand-wildcards (expand-file-name "*/*/platform.txt" file-name)
                              'full)))
    (mapcar #'file-name-directory)
    (mapcar
     (lambda (file-name)
       `((home        . ,file-name)
         (boards      . ,(gino--read-properties
                          (expand-file-name "boards.txt" file-name)))
         (platform    . ,(gino--read-properties
                          (expand-file-name "platform.txt" file-name)))
         (programmers . ,(gino--read-properties
                          (expand-file-name "programmers.txt" file-name))))))))

(defun gino--insert-section-heading (name)
  "Insert section heading into current.
NAME is a string."
  (unless (= (point) 1)
    (insert "\n"))
  (insert "[" name "]\n"))

(defun gino--normalize-property-value (value)
  "Normalize property value VALUE.
VALUE is a symbol, number, string or list.  Value is converted to
a string."
  (cond
   ((listp value)
    (format "[%s]" (mapconcat #'gino--normalize-property-value value ", ")))
   ((stringp value)
    (format "'%s'" value))
   (t
    (format "%s" value))))

(defun gino--insert-property (key value)
  "Insert property into current.
KEY is a string.  VALUE is a symbol, number, string or list."
  (insert key " = " (gino--normalize-property-value value) "\n"))

(eval-and-compile
  (defun gino--propertyp (element)
    "Return whether ELEMENT is a property."
    (not (keywordp element)))

  (defun gino--normalize-sections-spec (spec &optional sections)
    "Normalize sections specification SPEC.
SPEC is a list of section names and properties.  Specification is
converted to a nested alist.  Keys are converted to strings.
SECTIONS is used internally."
    (if spec
        (let ((section
               (cons
                (replace-regexp-in-string (rx line-start ":") ""
                                          (symbol-name (car spec)))
                (mapcar (pcase-lambda (`(,key ,value))
                          (cons (symbol-name key) value))
                        (seq-partition
                         (seq-take-while #'gino--propertyp (cdr spec)) 2)))))
          (gino--normalize-sections-spec
           (seq-drop-while #'gino--propertyp (cdr spec))
           (cons section sections)))
      (reverse sections))))

(defmacro gino--write-cross-file (file-name &rest spec)
  (declare (indent 1))
  `(with-temp-file ,file-name
     ,@(seq-mapcat (pcase-lambda (`(,name . ,properties))
                     (cons `(gino--insert-section-heading ,name)
                           (mapcar (pcase-lambda (`(,key . ,value))
                                     `(gino--insert-property ,key ,value))
                                   properties)))
                   (gino--normalize-sections-spec spec))))

;;;###autoload
(defun gino-generate-project (file-name board-spec)
  "Generate Arduino project.
FILE-NAME is a string, the file name to the project home
directory.  BOARD-SPEC is an alist, the board specification.  It
has two keys, hardware and board.  hardware points to a
hardware specification.  board points to a board in hardware."
  (interactive
   (list
    (thread-last (read-directory-name "Project home: ")
      expand-file-name
      file-name-as-directory)
    (let ((candidates
           (let ((hardware-specs
                  (seq-filter
                   (lambda (spec)
                     (seq-some (lambda (pred)
                                 (funcall pred spec))
                               gino-hardware-whitelist-preds))
                   (gino--read-hardware-specs gino-hardware-install-dirs))))
             (seq-mapcat
              (lambda (hardware-spec)
                (let ((platform-name
                       (format "%s %s"
                               (gino--get-in '(platform name) hardware-spec)
                               (gino--get-in '(platform version) hardware-spec))))
                  (thread-last (gino--get-in '(boards) hardware-spec)
                    (mapcar #'cdr)
                    (seq-filter (apply-partially #'gino--get-in '(name)))
                    (mapcar (lambda (board-spec)
                              (cons (format "%s (%s)"
                                            (gino--get-in '(name) board-spec)
                                            platform-name)
                                    `((board    . ,board-spec)
                                      (hardware . ,hardware-spec))))))))
              hardware-specs))))
      (cdr (assoc (completing-read "Board: " candidates nil t) candidates)))))
  (make-directory file-name 'parents)
  (gino--write-cross-file (expand-file-name "cross.txt" file-name)
    :binaries
    c (executable-find "avr-gcc")
    cpp (executable-find "avr-g++")
    :properties
    ino_src (directory-files
             (thread-last (gino--get-in '(hardware home) board-spec)
               (expand-file-name "cores/")
               (expand-file-name
                (gino--get-in '(board build core) board-spec))
               file-name-as-directory)
             'full (rx (or ".c" ".cpp") line-end))
    :host_machine
    system "bare"))

(provide 'gino)

;;; gino.el ends here
