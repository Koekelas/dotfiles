;;; prepcast.el --- Prepare for screencasting -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Nicolas De Jaeghere

;; Author: Nicolas De Jaeghere <nicolas@dejaeghe.re>
;; Keywords: faces
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

;; This package provides prepcast-mode, a global minor mode.  When
;; enabled, prepare for screencasting, when disabled, revert
;; preparations.  A preparation can be anything, from increasing the
;; default face height to launching a program.

;;; Code:

(require 'subr-x)
(require 'seq)

(defgroup prepcast nil
  "Prepare for screencasting."
  :group 'faces)

(defcustom prepcast-scale 1.25
  "Scale of elements."
  :type 'float)

(defcustom prepcast-prep-fs
  '(prepcast-prep-default-face prepcast-prep-moody prepcast-prep-keycast)
  "List of prepare functions.
A prepare function prepares an element for screencasting and must
return a function to revert the preparation or, to not revert the
preparation, nil."
  :type 'function)

(defvar prepcast--unprep-fs nil
  "List of unprepare functions.")

(defun prepcast-prep-default-face ()
  "Prepare default face for screencasting."
  (let ((height (face-attribute 'default :height)))
    (set-face-attribute 'default nil :height (round (* height prepcast-scale)))
    (lambda ()
      (set-face-attribute 'default nil :height height))))

(defun prepcast-prep-moody ()
  "Prepare moody for screencasting."
  (when (boundp 'moody-mode-line-height)
    (let ((height moody-mode-line-height))
      (setq moody-mode-line-height (round (* height prepcast-scale)))
      (lambda ()
        (setq moody-mode-line-height height)))))

(declare-function keycast-mode "ext:keycast")
(defvar keycast-mode)

(defun prepcast-prep-keycast ()
  "Prepare keycast for screencasting."
  (when (fboundp 'keycast-mode)
    (unless keycast-mode
      (keycast-mode 1)
      (lambda ()
        (keycast-mode 0)))))

;;;###autoload
(define-minor-mode prepcast-mode
  "Prepare for screencasting."
  :global t
  (if prepcast-mode
      (unless prepcast--unprep-fs
        (setq prepcast--unprep-fs
              (thread-last prepcast-prep-fs
                (mapcar #'funcall)
                (seq-filter #'identity))))
    (mapc #'funcall prepcast--unprep-fs)
    (setq prepcast--unprep-fs nil)))

(provide 'prepcast)

;;; prepcast.el ends here
