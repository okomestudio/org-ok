;;; op-ok-org.el --- Org Plugin  -*- lexical-binding: t -*-
;;
;; Copyright (C) 2024 Taro Sato
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
;;; Code:

(require 'org)

;;; EMPHASIS
;;
;; See https://stackoverflow.com/a/24540651/515392
;;
(let ((regexp-components
       `(;; pre match
         ,(concat (string ?\[ ?\( ?{)
                  "[:space:][:multibyte:]"
                  (string ?\N{ZERO WIDTH SPACE}
                          ?' ?‘ ?\" ?“
                          ?| ?│
                          ?— ?-))       ; "-" must be the last char
         ;; post match
         ,(concat (string ?\] ?\) ?})   ; "]" must be the first char
                  "[:space:][:multibyte:]"
                  (string ?\N{ZERO WIDTH SPACE}
                          ?' ?’ ?\" ?”
                          ?| ?│
                          ?. ?, ?? ?! ?\; ?:
                          ?s            ; allow use like =def=s
                          ?— ?-))       ; "-" must be the last char
         "[:space:]"                    ; forbidden border chars
         "."                            ; body "."
         1)))                           ; max newlines
  ;; See `org-emph-re' and `org-verbatim-re' for the final regexps
  (org-set-emph-re 'org-emphasis-regexp-components regexp-components))

;;; Misc.

(defun op-ok-org-add-properties (props)
  "Add properties.
PROPS is a list of cons cells (keyword . values). When values is
a list, its values are used as fixed values for the preset
properties."
  (dolist (prop props)
    (let ((keyword (car prop))
          (values (cdr prop)))
      (when values
        (add-to-list 'org-global-properties-fixed
                     `(,(concat keyword "_ALL")
                       .
                       ,(mapconcat (lambda (s)
                                     (format "\"%s\"" s))
                                   values
                                   " "))))
      (add-to-list 'org-default-properties keyword))))

(provide 'op-ok-org)
;;; op-ok-org.el ends here
