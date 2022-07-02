;;; belgian-holidays --- Belgian holidays -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2022 Nicolas De Jaeghere

;; Author: Nicolas De Jaeghere <nicolas@dejaeghe.re>
;; Keywords: calendar
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

;; This package defines the Belgian holidays. It's intended to be used
;; with the builtin holidays package. The holiday names are in Dutch.
;; To configure belgian-holidays, add to your initialization file:
;;
;;     (require 'holidays)
;;     (require 'belgian-holidays)
;;
;;     ;; Either both public and miscellaneous holidays
;;     (setq holiday-other-holidays
;;           (append holiday-other-holidays holiday-belgian-holidays))
;;
;;     ;; Or only public holidays
;;     (setq holiday-other-holidays
;;           (append holiday-other-holidays holiday-belgian-public-holidays))

;;; Code:

(defvar holiday-belgian-public-holidays
  '((holiday-fixed 1 1 "Nieuwjaar")
    (holiday-easter-etc 0 "Pasen")
    (holiday-easter-etc 1 "Paasmaandag")
    (holiday-fixed 5 1 "Dag van de Arbeid")
    (holiday-easter-etc 39 "Onze-Lieve-Heer-Hemelvaart")
    (holiday-easter-etc 49 "Pinksteren")
    (holiday-easter-etc 50 "Pinkstermaandag")
    (holiday-fixed 7 21 "Nationale feestdag van België")
    (holiday-fixed 8 15 "Onze-Lieve-Vrouw-Hemelvaart")
    (holiday-fixed 11 1 "Allerheiligen")
    (holiday-fixed 11 11 "Wapenstilstand")
    (holiday-fixed 12 25 "Kerstmis"))
  "List of public Belgian holiday expressions.")

(defvar holiday-belgian-misc-holidays
  '((holiday-fixed 1 6 "Driekoningen")
    (holiday-fixed 2 14 "Valentijnsdag")
    (holiday-fixed 3 19 "Vaderdag (Antwerpen)")
    (holiday-float 5 0 2 "Moederdag")
    (holiday-float 6 0 2 "Vaderdag")
    (holiday-fixed 7 11 "Feestdag van Vlaanderen")
    (holiday-fixed 8 15 "Moederdag (Antwerpen)")
    (holiday-fixed 9 27 "Dag van de Franse Gemeenschap")
    (holiday-fixed 11 2 "Allerzielen")
    (holiday-fixed 11 11 "Sint-Maarten")
    (holiday-fixed 11 15 "Dag van de Duitstalige Gemeenschap")
    (holiday-fixed 11 15 "Koningsdag")
    (holiday-fixed 12 6 "Sinterklaas")
    (holiday-fixed 12 26 "Tweede kerstdag")
    (holiday-fixed 12 31 "Oudejaarsavond"))
  "List of miscellaneous Belgian holiday expressions.")

(defvar holiday-belgian-holidays
  (append holiday-belgian-public-holidays holiday-belgian-misc-holidays)
  "List of Belgian holiday expressions.")

(provide 'belgian-holidays)

;;; belgian-holidays ends here
