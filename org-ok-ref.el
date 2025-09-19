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

;;; MULE utility

(defvar ok-mule-s nil)

(defun ok-mule-lang-p (lang)
  "TBD."
  (if (eq lang 'ja)
      current-input-method))

(defun ok-mule-s (text &optional lang)
  "Render TEXT in the current language.
If LANG, a two-char symbol, is not given, it will be auto-discovered, currently
`ja' only. The default is `en'."
  (let ((lang (or lang (if current-input-method 'ja 'en))))
    (alist-get lang (alist-get text ok-mule-s nil nil #'equal) text)))

;;;

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

(push '("%s et al." . ((ja . "%s他"))) ok-mule-s)
(push '("%s & %s" . ((ja . "%s＆%s"))) ok-mule-s)
(push '("%s %s" . ((ja . "%s%s"))) ok-mule-s)

(defun org-ok-ref-name--etal (names)
  "Format NAMES with et al."
  (cond ((= (length names) 1)
         (nth 0 names))
        ((= (length names) 2)
         (format (ok-mule-s "%s & %s")
                 (nth 0 names) (nth 1 names)))
        ((>= (length names) 3)
         (format (ok-mule-s "%s et al.")
                 (nth 0 names)))
        (t "")))

(defun org-ok-ref-bibtex--sanitize-title (title)
  "Sanitize string TITLE for display."
  (setq title (replace-regexp-in-string "{{\\([^}]+\\)}}" "\\1" title))
  (replace-regexp-in-string "\\\\&" "&" title))

(defun org-ok-ref-bibtex--prep-person-name (s)
  "Preprocess person name in string S for internal use."
  (when s
    (--map
     (if (string-match "\\(.*\\), \\(.*\\)\\s-*" it)
         (let ((last-name (match-string 1 it))
               (first-name (match-string 2 it)))
           (cons last-name first-name))
       (cons it nil))
     (s-split " and " (replace-regexp-in-string "{\\([^}]+\\)}" "\\1" s)))))

(defun org-ok-ref-bibtex--langid-to-lang (langid)
  "Convert string LANGID to lang, a symbol."
  (pcase langid
    ("english"  'en)
    ("japanese" 'ja)
    (_ 'en)))

(defun org-ok-ref-bibtex-entry (key)
  "Format the BibTeX entry with KEY for use with format string."
  (let* ((entry (bibtex-completion-get-entry key))
         (lang (org-ok-ref-bibtex--langid-to-lang
                (bibtex-completion-get-value "langid" entry)))
         (authors (org-ok-ref-bibtex--prep-person-name
                   (bibtex-completion-get-value "author" entry)))
         (editors (org-ok-ref-bibtex--prep-person-name
                   (bibtex-completion-get-value "editor" entry)))
         (title (org-ok-ref-bibtex--sanitize-title
                 (bibtex-completion-get-value "title" entry)))
         (location (bibtex-completion-get-value "location" entry))
         (publisher (bibtex-completion-get-value "publisher" entry ""))
         (year (bibtex-completion-get-value "date" entry "N/A"))
         authors-full)
    (list (cons "authors" (org-ok-ref-name--etal (--map (car it) authors)))
          (cons "authors-full"
                (org-ok-ref-name--etal
                 (--map (if-let* ((ln (car it)) (fn (cdr it)))
                            (apply #'format
                                   (pcase lang
                                     ('ja `("%s%s" ,ln ,fn))
                                     (_ `("%s %s" ,fn ,ln))))
                          (car it))
                        authors)))
          (cons "editors" (org-ok-ref-name--etal (--map (car it) editors)))
          (cons "editors-full"
                (org-ok-ref-name--etal
                 (--map (if-let* ((ln (car it)) (fn (cdr it)))
                            (apply #'format
                                   (pcase lang
                                     ('ja `("%s%s" ,ln ,fn))
                                     (_ `("%s %s" ,fn ,ln))))
                          (car it))
                        editors)))
          (cons "title" title)
          (cons "location" location)
          (cons "publisher" publisher)
          (cons "year" (if (string-match "\\([0-9]\\{4\\}\\)" year)
                           (match-string 1 year)
                         year)))))

(defun org-ok-ref-format-string--apa (key entry)
  "Render the BibTeX entry with KEY in the APA style.
ENTRY is created by `org-ok-ref-bibtex-entry'."
  (format "[[${citekey}][%s]]" (bibtex-completion-apa-format-reference key)))

(defun org-ok-ref-format-string--chicago-in-text (key entry)
  "Render the BibTeX entry with KEY in a in-text Chicago style.
ENTRY is created by `org-ok-ref-bibtex-entry'."
  (ok-mule-s
   (concat (if (and (s-blank? (cdr (assoc-string "authors-full" entry)))
                    (assoc-string "editors-full" entry))
               "${editors-full} (ed.), "
             "${authors-full}, ")
           "/[[${citekey}][${title}]]/ (${location}: ${publisher}, ${year})")))

(push '("${authors-full}, /[[${citekey}][${title}]]/ (${location}: ${publisher}, ${year})"
        . ((ja . "${authors-full}『[[${citekey}][${title}]]』（${publisher}、${year}）")))
      ok-mule-s)
(push '("${editors-full} (ed.), /[[${citekey}][${title}]]/ (${location}: ${publisher}, ${year})"
        . ((ja . "${editors-full}編『[[${citekey}][${title}]]』（${publisher}、${year}）")))
      ok-mule-s)

(defvar org-ok-ref-format-string
  '((book
     . ((apa . org-ok-ref-format-string--apa)
        (author-year-paren . "[[${citekey}][${authors} (${year})]]")
        (author-year . "[[${citekey}][${authors} ${year}]]")
        (full-inline . org-ok-ref-format-string--chicago-in-text)))
    (_
     . ((apa . org-ok-ref-format-string--apa)
        (author-year-paren . "[[${citekey}][${authors} (${year})]]")
        (author-year . "[[${citekey}][${authors} ${year}]]")
        (full-inline . org-ok-ref-format-string--chicago-in-text))))
  "String formats for BibTeX entry types.")

(defun org-ok-ref-link-insert (&optional _arg)
  "Insert or update a cite link with a reference description.
When the point is on an existing link, the citekey is used to get the reference
key. Otherwise, the command will prompt for a key.

When the point is on an existing link, the description will be updated based on
the user request. Otherwise, a new link will be created.

Use the prefix argument to choose the style for description:

  - 1: 'Authors (Year)'
  - 2: 'Authors Year'
  - 3: APA
  - 4: Trigger prompt for interactive selection

The default style is 'Authors _Title_ (Publisher, Year)'."
  (interactive "P")
  (let* ((style
          (pcase _arg
            ('1 'author-year-paren)
            ('2 'author-year)
            ('3 'apa)
            ((or '4 '(4))
             (let ((collection
                    '(("APA" . apa)
                      ("Authors _Title_ (Publisher, Year)" . full-inline)
                      ("Authors (Year)" . author-year-paren)
                      ("Authors Year" . author-year))))
               (alist-get (completing-read "Reference style: "
                                           collection nil t)
                          collection nil nil #'equal)))
            (_ 'full-inline)))
         (range nil)
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
         (entry (org-ok-ref-bibtex-entry key))
         (fstring
          (alist-get style
                     (alist-get (bibtex-completion-get-value "=type=" entry)
                                org-ok-ref-format-string
                                (alist-get '_ org-ok-ref-format-string)))))
    (push `("citekey" . ,(format "cite:&%s" key)) entry)

    ;; Delete the existing link
    (when-let* ((start (car range)) (end (cadr range)))
      (delete-region start end)
      (goto-char start))

    (insert (s-format (if (stringp fstring) fstring (funcall fstring key entry))
                      (lambda (field entry) (cdr (assoc-string field entry)))
                      entry))))

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
