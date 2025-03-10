;;; org-ok-ref.el --- Org Ref Plugin  -*- lexical-binding: t -*-
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
;; The plugin for `org-ref'.
;;
;;; Code:

(require 'org-ref)

(defun org-ok-ref--define-accessors (record)
  "Define Bibtex field accessors for RECORD."
  (dolist (field (mapcar (lambda (e) (car e)) record))
    (let ((name (concat "org-ok-ref-record-" field)))
      (defalias (intern name)
        (lambda (&rest _)
          (assoc-default field record))
        (format "Get field '%s' from the Bibtex record." field))))

  (defun org-ok-ref-record-slug (&optional key)
    "Get the slug for the current Bibtex record."
    (org-ok-text-to-slug (org-ok-ref-record-title key)))

  (defun org-ok-ref-record-title (&optional key)
    "Get the title for the current Bibtex record."
    (let ((title (org-ok-ref-record--field "title" key)))
      (string-replace "}}" "" (string-replace "{{" "" title)))))

(defun org-ok-ref-prompt ()
  "Prompt for a Bibtex key."
  (setq org-ok-ref--key (org-ref-read-key)
        org-ok-ref--record (bibtex-completion-get-entry org-ok-ref--key))
  (org-ok-ref--define-accessors org-ok-ref--record)
  org-ok-ref--record)

(defun org-ok-ref-record--field (field &optional key)
  "Get the value for FIELD in the current bibtex record."
  (assoc-default field (or (and key (bibtex-completion-get-entry key))
                           org-ok-ref--record)))

(defun org-ok-ref-record-citekey (&optional key)
  "Get the cite key for KEY.
If not supplied, the stored key is used."
  (format "cite:&%s" (or key org-ok-ref--key)))

(make-obsolete 'org-ok-ref--define-accessors 'org-roam-ok-capture "2025-03-09")
(make-obsolete 'org-ok-ref-prompt 'org-roam-ok-capture "2025-03-09")
(make-obsolete 'org-ok-ref-record--field 'org-roam-ok-capture "2025-03-09")
(make-obsolete 'org-ok-ref-record-citekey 'org-roam-ok-capture "2025-03-09")

(provide 'org-ok-ref)
;;; org-ok-ref.el ends here
