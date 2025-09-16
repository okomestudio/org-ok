;;; org-ok-ref.el --- Org Ref Plugin  -*- lexical-binding: t -*-
;;
;; Copyright (C) 2025 Taro Sato
;;
;;; License:
;;
;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along with
;; this program. If not, see <https://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; The plugin for `org-ref'.
;;
;;; Code:

(require 'dash)
(require 'ok)
(require 'org-ref)
(require 's)

(defun org-ok-ref-format-author (authors)
  "Format BibTeX AUTHORS list.
See `bibtex-completion-shorten-authors' for reference."
  (cl-loop for a in (s-split " and " authors)
           for p = (--map (s-trim it) (s-split "," a t))
           for sep = "" then ", "
           concat sep
           if (eq 1 (length p))
           concat (car p)
           else
           concat (if (ok-string-contains-ja-p (car p))
                      (concat (car p) (cadr p))
                    (concat (cadr p) " " (car p)))))

(defun org-ok-ref-title--sanitize (title)
  "Sanitize string TITLE for display."
  (setq title (replace-regexp-in-string "{{\\([^}]+\\)}}" "\\1" title))
  (setq title (replace-regexp-in-string "\\\\&" "&" title))
  title)

(defun org-ok-ref-link-description (key style)
  "Render link description for cite KEY in STYLE."
  (let* ((bibentry (bibtex-completion-get-entry key))
         (authors (or (bibtex-completion-get-value "author" bibentry)
                      (bibtex-completion-get-value "editor" bibentry)))
         (year (bibtex-completion-get-value "date" bibentry "N/A")))
    (setq authors (--map (if (string-match "\\(.*\\),.*" it)
                             (format "%s" (match-string 1 it))
                           it)
                         (split-string authors " and ")))
    (setq authors (cond ((= (length authors) 1)
                         (nth 0 authors))
                        ((= (length authors) 2)
                         (format "%s & %s" (nth 0 authors) (nth 1 authors)))
                        ((>= (length authors) 3)
                         (format "%s et al." (nth 0 authors)))
                        (t "N/A")))
    (setq year (if (string-match "\\([0-9]\\{4\\}\\)" year)
                   (match-string 1 year)
                 year))
    (pcase style
      ('title (org-ok-ref-title--sanitize
               (bibtex-completion-get-value "title" bibentry)))
      ('short-paren (format "%s (%s)" authors year))
      ('short (format "%s %s" authors year))
      ('long (format "%s" authors))
      ('full (bibtex-completion-apa-format-reference key)))))

(defun org-ok-ref-link-insert (&optional _arg)
  "Insert or update a cite link.
This command insert a new link or updates an existing link if the point is on a
link.

The prefix argument sets the style for `org-ok-ref-link-description':

- 1: title
- 4: short-paren
- 16: short
- 64: long

See the help for `org-ok-ref-link-description' for the descriptions of available styles."
  (interactive "P")
  (let* (range
         (key
          (if-let*
              ((link (and (org-in-regexp org-link-any-re)
                          (substring-no-properties (match-string 0))))
               (start (match-beginning 0))
               (end (match-end 0))
               (key (and (string-match "^\\[\\[cite:&\\([^]]*\\)\\].*" link)
                         (match-string 1 link))))
              (progn
                (setq range (list start end))
                key)
            (org-ref-read-key)))
         (style (pcase _arg
                  ('1 'title)
                  ('4 'short-paren) ('(4) 'short-paren)
                  ('16 'short) ('(16) 'short)
                  ('64 'long) ('(64) 'long)
                  ('256 'full) ('(256) 'full))))
    (when-let* ((start (car range))
                (end (cadr range)))
      (delete-region start end)
      (goto-char start))
    (org-insert-link nil
                     (format "cite:&%s" key)
                     (org-ok-ref-link-description key style))))

;;; Obsolete Utility Functions
;;
;; These functions were obsoleted when the feature needing them was consolidated
;; and moved to `org-roam-ok-capture'.

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
    (ok-string-text-to-slug (org-ok-ref-record-title key)))

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
