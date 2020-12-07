;;; gino.el --- Generate Arduino project -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2023 Nicolas De Jaeghere

;; Author: Nicolas De Jaeghere <nicolas@dejaeghe.re>
;; Keywords: tools
;; URL: <https://github.com/Koekelas/dotfiles>

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

;; Generate Arduino project

;;; Code:

(defun gino--get-in (plist &rest keys)
  (if keys
      (apply #'gino--get-in (plist-get plist (car keys)) (cdr keys))
    plist))

(defun gino--put-in (plist &rest args)
  (pcase-let ((`(,value . ,rem) args))
    (if rem
        (plist-put plist value
                   (apply #'gino--put-in (plist-get plist value) rem))
      value)))

(provide 'gino)

;;; gino.el ends here
