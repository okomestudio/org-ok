;;; org-ok-utils.el --- Org Plugin Utilities  -*- lexical-binding: t -*-
;;
;; Copyright (C) 2025 Taro Sato
;;
;;; License:
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; The utility functions for Org plugin.
;;
;;; Code:

(require 'dash)

(defun org-ok-text-to-slug (text)
  "Turn TEXT into its '-'-delimited slug.

This function is used in place of `org-roam-node-slug'."
  (let (;; Combining Diacritical Marks
        ;; https://www.unicode.org/charts/PDF/U0300.pdf
        (slug-trim-chars '(768  ; U+0300 COMBINING GRAVE ACCENT
                           769  ; U+0301 COMBINING ACUTE ACCENT
                           770  ; U+0302 COMBINING CIRCUMFLEX ACCENT
                           771  ; U+0303 COMBINING TILDE
                           772  ; U+0304 COMBINING MACRON
                           774  ; U+0306 COMBINING BREVE
                           775  ; U+0307 COMBINING DOT ABOVE
                           776  ; U+0308 COMBINING DIAERESIS
                           777  ; U+0309 COMBINING HOOK ABOVE
                           778  ; U+030A COMBINING RING ABOVE
                           779  ; U+030B COMBINING DOUBLE ACUTE ACCENT
                           780  ; U+030C COMBINING CARON
                           795  ; U+031B COMBINING HORN
                           803  ; U+0323 COMBINING DOT BELOW
                           804  ; U+0324 COMBINING DIAERESIS BELOW
                           805  ; U+0325 COMBINING RING BELOW
                           807  ; U+0327 COMBINING CEDILLA
                           813 ; U+032D COMBINING CIRCUMFLEX ACCENT BELOW
                           814 ; U+032E COMBINING BREVE BELOW
                           816 ; U+0330 COMBINING TILDE BELOW
                           817)))      ; U+0331 COMBINING MACRON BELOW
    (cl-flet* ((nonspacing-mark-p (char)
                 (memq char slug-trim-chars))
               (strip-nonspacing-marks (s)
                 (string-glyph-compose
                  (apply #'string
                         (seq-remove #'nonspacing-mark-p
                                     (string-glyph-decompose s)))))
               (cl-replace (text pair)
                 (replace-regexp-in-string (car pair) (cdr pair) text)))
      (let* ((pairs `(             ; convert anything not alphanumeric
                      ("[^[:alnum:][:digit:]]" . "-")

                      ("--*" . "-")    ; remove sequential underscores
                      ("^-" . "")      ; remove starting underscore
                      ("-$" . "")))    ; remove ending underscore
             (slug (-reduce-from #'cl-replace
                                 (strip-nonspacing-marks text)
                                 pairs)))
        (downcase slug)))))

(provide 'org-ok-utils)
;;; org-ok-utils.el ends here
