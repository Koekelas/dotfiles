;;; koek-subr.el --- Common subroutines -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2022 Nicolas De Jaeghere

;; Author: Nicolas De Jaeghere <nicolas@dejaeghe.re>
;; Keywords: lisp
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

;; This package provides common subroutines for use with Nicolas'
;; Emacs configuration.

;;; Code:

(eval-when-compile
  (require 'subr-x))
(require 'seq)

;;; Configuration variables

(defvar koek-subr/roman-numerals
  '((1000 . "M")
    ( 900 . "CM")
    ( 500 . "D")
    ( 400 . "CD")
    ( 100 . "C")
    (  90 . "XC")
    (  50 . "L")
    (  40 . "XL")
    (  10 . "X")
    (   9 . "IX")
    (   5 . "V")
    (   4 . "IV")
    (   1 . "I"))
  "Alist of Arabic numeral to Roman numeral pairs.
Pairs are sorted from largest to smallest.")

(defvar koek-subr/chevrons
  (rx (any "<" ">" "\N{LEFT ANGLE BRACKET}" "\N{RIGHT ANGLE BRACKET}"
           "\N{SINGLE LEFT-POINTING ANGLE QUOTATION MARK}"
           "\N{SINGLE RIGHT-POINTING ANGLE QUOTATION MARK}"
           "\N{LEFT-POINTING DOUBLE ANGLE QUOTATION MARK}"
           "\N{RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK}"))
  "Regular expression matching chevrons.")

(defvar koek-subr/down-words
  (rx word-start
      (or "a" "an" "and" "as" "at" "but" "by" "en" "for" "if" "in" "nor" "of"
          "on" "or" "per" "the" "to" "v" "v." "via" "vs" "vs.")
      word-end)
  "Regular expression matching words down cased in title case.")

;;; Number subroutines

(defun koek-subr/arabic-to-roman (n &optional roman-numerals)
  "Convert Arabic number N to a Roman number.
N is an integer greater than zero.  ROMAN-NUMERALS is used
internally."
  (unless roman-numerals
    (setq roman-numerals koek-subr/roman-numerals))
  (when (> n 0)
    (pcase-let ((`(,arabic . ,roman) (car roman-numerals)))
      (if (>= n arabic)
          (concat roman (koek-subr/arabic-to-roman (- n arabic) roman-numerals))
        (koek-subr/arabic-to-roman n (cdr roman-numerals))))))

;;; String subroutines

(defun koek-subr/strip-chevrons (s)
  "Strip chevrons from beginning and ending of string S.
S is a string, the string to strip chevrons from."
  (let ((chevrons (rx (one-or-more (regexp koek-subr/chevrons)))))
    (string-trim s chevrons chevrons)))

;; The algorithm is from https://github.com/gouch/to-title-case
(defun koek-subr/title-case (title)
  "Title case TITLE.
TITLE is a string, the title to title case."
  (let* ((pieces (koek-subr/cut title (rx (or (any ":-") (one-or-more blank)))))
         (next-window (koek-subr/make-sliding-window pieces 5 'center))
         (window nil)
         (parts nil))
    (while (setq window (funcall next-window))
      (pcase-let ((`(,pprev ,prev ,cur ,next ,nnext) window))
        (cond
         ;; ---------- 1. Down word? Down case current? ----------
         ((and
           ;;   I. Down word?
           (let ((case-fold-search t))  ; Dynamic variable
             (string-match koek-subr/down-words cur))
           ;;  II. Not begin/end of title?
           prev next
           ;; III. Not end of main title/begin of sub title?
           (not (string-equal next ":")) (not (string-equal (or pprev "") ":"))
           ;;  IV. Not begin/end of hyphenated word?
           (or (not (string-equal next "-"))
               (and (string-equal prev "-") (string-equal next "-"))))
          (push (downcase cur) parts))
         ;; ---------- 2. Deliberately cased? Keep case current? ----------
         ((or
           ;;   I. Up case other than first character?
           (let ((case-fold-search nil)) ; Dynamic Variable
             (string-match (rx not-newline upper) cur))
           ;;  II. URI scheme?
           (and (string-equal (or next "") ":")
                (not (string-blank-p (or nnext ""))))
           ;; III. URI host?
           (string-match (rx not-newline "." not-newline) cur))
          (push cur parts))
         ;; ---------- 3. Default, capital case current ----------
         (t
          (push (capitalize cur) parts)))))
    (string-join (reverse parts))))

(defun koek-subr/normalize-title (title)
  "Join lines, collapse whitespace and title case TITLE.
TITLE is a string, the title to normalize."
  (let* ((titles
          (split-string
           (replace-regexp-in-string (rx (one-or-more blank)) " " title)
           (rx (one-or-more "\n")) 'omit-nulls " "))
         (main (car titles))
         (subs (cdr titles))
         (normalized
          (concat main (when subs (concat ": " (string-join subs " "))))))
    (koek-subr/title-case normalized)))

(defun koek-subr/elide (s length)
  "Elide string S to LENGTH.
S is a string, the string to elide.  LENGTH is an integer, the
maximum length of S including the elide character."
  (truncate-string-to-width s length nil nil t))

(defun koek-subr/cut (s where)
  "Cut string S into pieces at every WHERE.
S is a string, the string to cut.  WHERE is a regular expression,
where to cut S.  When WHERE matches the empty string (e.g. word
boundary), cut S at match, else, cut S before and after match.
When WHERE specifies capture groups, cut S at first matching
capture group.

Unlike `split-string', calling `string-join' on result reproduces
S, i.e., separators are part of result."
  (let* ((length (length s))
         (start 0)
         (poss (list start)))
    (while (and (< start length) (string-match where s start))
      (pcase-let
          ((`(,begin ,end)
            (let ((matches (seq-filter #'car (seq-partition (match-data) 2))))
              (or (cadr matches) (car matches)))))
        (unless (= begin (car poss))
          (push begin poss))
        (unless (= end begin)
          (push end poss))
        (setq start (if (= start end) (1+ end) end))))
    (unless (= length (car poss))
      (push length poss))
    (setq poss (reverse poss))
    (seq-mapn (apply-partially #'substring s) poss (cdr poss))))

;;; List subroutines

(defun koek-subr/make-sliding-window (xs width &optional align)
  "Return generator function to slide over XS.
Slide over list XS, from begin to end, element by element,
generating sublists/windows of next WIDTH elements.  Calling
generator returns next window or, when end is reached, nil.

XS is a list, the elements to slide over.  WIDTH is an integer,
the number of elements per window.  Optional ALIGN is a symbol,
how to align the elements in the window, either nil (don't pad
XS), center (pad XS so first element is in center of first
window, last element in center last window) or border (pad XS so
first element is at end of first window, last element at begin
last window) and defaults to nil.  For center, WIDTH must be
odd."
  (let* ((n-pad (pcase align
                  ('center (/ (1- width) 2))
                  ('border (1- width))
                  (_align  0)))
         (rem (append (make-list n-pad nil) xs (make-list n-pad nil)))
         (n-rem (length rem)))
    (lambda ()
      (when (>= n-rem width)
        (prog1 (seq-take rem width)
          (setq n-rem (1- n-rem))
          (setq rem (cdr rem)))))))

;;; Buffer subroutines

(defun koek-subr/construct-earmuffed-name (&rest parts)
  (let* ((names (thread-last parts
                  (remq nil)
                  (mapcar (apply-partially #'format "%s"))
                  (remove "")))
         (main (car names))
         (subs (cdr names)))
    (concat "*" main (when subs (concat ": " (string-join subs " "))) "*")))

;; project
(defun koek-subr/reset-default-directory ()
  (unless (buffer-file-name)
    (setq default-directory "~/")))

;;; File name subroutines

(defun koek-subr/lock-file-p (file-name)
  "Return whether FILE-NAME is a lock file."
  (string-prefix-p ".#" (file-name-nondirectory file-name)))

(defun koek-subr/get-child-dirs (file-name &optional full)
  "Return child directories in directory FILE-NAME.
When optional FULL is truthy, return absolute file names, else,
return relative file names."
  (thread-last (directory-files-and-attributes file-name full)
    (seq-filter (pcase-lambda (`(,file-name ,type))
                  (let ((name (file-name-nondirectory file-name)))
                    (and (eq type t)    ; Directory
                         (string-match directory-files-no-dot-files-regexp
                                       name)))))
    (mapcar (pcase-lambda (`(,file-name))
              (file-name-as-directory file-name)))))

;;; URI subroutines

(defun koek-subr/urip (s)
  "Return whether S is a URI.
S is a string, the string to interrogate."
  (when s
    (string-match
     (rx line-start alpha (zero-or-more (any alnum "+-.")) ":") s)))

(provide 'koek-subr)

;;; koek-subr.el ends here
