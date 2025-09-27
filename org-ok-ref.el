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
(require 'mulex)
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

(defun org-ok-ref-bibtex--name-full (name lang)
  "Format NAME for display in LANG."
  (if-let* ((last-name (car name)) (first-name (cdr name)))
      (apply #'format (pcase lang
                        ('ja `("%s%s" ,last-name ,first-name))
                        (_ `("%s %s" ,first-name ,last-name))))
    (car name)))

(defun org-ok-ref-bibtex--name-etal (names)
  "Format NAMES with et al."
  (cond ((= (length names) 1)
         (nth 0 names))
        ((= (length names) 2)
         (format (mulex-s "%s & %s" '((ja . "%s＆%s")))
                 (nth 0 names) (nth 1 names)))
        ((>= (length names) 3)
         (format (mulex-s "%s et al." '((ja . "%s他")))
                 (nth 0 names)))))

(defun org-ok-ref-bibtex--prep-date (s-date)
  "Preprocess string date S-DATE."
  (mulex-date-decode s-date))

(defalias #'org-ok-ref-bibtex--prep-urldate #'org-ok-ref-bibtex--prep-date)

(defun org-ok-ref-bibtex--prep-langid (langid)
  "Preprocess string LANGID to lang."
  (alist-get langid mulex-languages nil nil #'equal))

(defun org-ok-ref-bibtex--prep-person-name (s)
  "Preprocess person names in string S."
  (when s
    (--map
     (if (string-match "\\(.*\\), \\(.*\\)\\s-*" it)
         (let ((last-name (match-string 1 it))
               (first-name (match-string 2 it)))
           (cons last-name first-name))
       (cons it nil))
     (s-split " and " (replace-regexp-in-string "{\\([^}]+\\)}" "\\1" s)))))

(defalias #'org-ok-ref-bibtex--prep-author #'org-ok-ref-bibtex--prep-person-name)
(defalias #'org-ok-ref-bibtex--prep-editor #'org-ok-ref-bibtex--prep-person-name)

(defun org-ok-ref-bibtex--prep-publisher (publisher)
  "Preprocess string PUBLISHER."
  (setq publisher (replace-regexp-in-string "{{\\([^}]+\\)}}" "\\1" publisher))
  (replace-regexp-in-string "\\\\&" "&" publisher))

(defun org-ok-ref-bibtex--prep-title (title)
  "Preprocess string TITLE."
  (setq title (replace-regexp-in-string "{{\\([^}]+\\)}}" "\\1" title))
  (replace-regexp-in-string "\\\\&" "&" title))

(defun org-ok-ref-bibtex--get (field entry &optional default)
  "Get the value for FIELD in BibTeX ENTRY.
DEFAULT is returned if the value for FIELD does not exist."
  (let ((result (bibtex-completion-get-value field entry default))
        (prep-fun (intern (concat "org-ok-ref-bibtex--prep-" field))))
    (if (fboundp prep-fun) (funcall prep-fun result) result)))

(defun org-ok-ref-bibtex-entry (key)
  "Format the BibTeX entry with KEY for use with format string."
  (let* ((entry (bibtex-completion-get-entry key))
         (type (org-ok-ref-bibtex--get "=type=" entry))
         (subtype (org-ok-ref-bibtex--get "entrysubtype" entry))
         (lang (org-ok-ref-bibtex--get "langid" entry))
         (title (org-ok-ref-bibtex--get "title" entry))
         (authors (org-ok-ref-bibtex--get "author" entry))
         (editors (org-ok-ref-bibtex--get "editor" entry))
         (date (org-ok-ref-bibtex--get "date" entry)))
    (append
     (list (cons 'citekey (format "cite:&%s" key))
           (cons 'type type)
           (cons 'lang lang)
           (cons 'title title)
           (cons 'authors (org-ok-ref-bibtex--name-etal (-map #'car authors)))
           (cons 'authors-full (org-ok-ref-bibtex--name-etal
                                (--map (org-ok-ref-bibtex--name-full it lang)
                                       authors)))
           (cons 'date date)
           (cons 'year (decoded-time-year date)))
     (cond
      ((and (string= type "article") (string= subtype "magazine"))
       (list (cons 'subtype subtype)
             (cons 'journaltitle (org-ok-ref-bibtex--get "journaltitle" entry))
             (cons 'month (decoded-time-month date))
             (cons 'day (decoded-time-day date))
             (cons 'pages (s-join "-"
                                  (s-split "--"
                                           (org-ok-ref-bibtex--get "pages" entry))))))
      ((string= type "online")
       (list (cons 'organization (org-ok-ref-bibtex--get "organization" entry))
             (cons 'month (decoded-time-month date))
             (cons 'day (decoded-time-day date))
             (cons 'url (org-ok-ref-bibtex--get "url" entry))
             (cons 'urldate (org-ok-ref-bibtex--get "urldate" entry))))
      (t
       (list (cons 'subtype subtype)
             (cons 'editors (org-ok-ref-bibtex--name-etal (-map #'car editors)))
             (cons 'editors-full (org-ok-ref-bibtex--name-etal
                                  (--map (org-ok-ref-bibtex--name-full it lang)
                                         editors)))
             (cons 'location (org-ok-ref-bibtex--get "location" entry ""))
             (cons 'publisher (org-ok-ref-bibtex--get "publisher" entry ""))))))))

(defun org-ok-ref-format-string--apa (key entry)
  "Render the BibTeX entry with KEY in the APA style.
ENTRY is created by `org-ok-ref-bibtex-entry'."
  (format "[[${citekey}][%s]]" (bibtex-completion-apa-format-reference key)))

(defun org-ok-ref-format-string--chicago-in-text (key entry)
  "Render the BibTeX entry with KEY in a in-text Chicago style.
ENTRY is created by `org-ok-ref-bibtex-entry'."
  (apply
   #'mulex-s
   (cond
    ((and (string= (alist-get 'type entry) "article")
          (string= (alist-get 'subtype entry) "magazine"))
     '("${authors-full}, “[[${citekey}][${title}]]” (/${journaltitle}/, ${date}, ${pages})"
       ((ja . "${authors-full}「[[${citekey}][${title}]]」（/${journaltitle}/、${date}、${pages}）"))))
    ((string= (alist-get 'type entry) "online")
     '("${authors-full} (${year}), “[[${citekey}][${title}]]” (${organization}, Last Accessed ${date-accessed}, [[${url}]])"

       ((ja . "${authors-full}（${year}）「[[${citekey}][${title}]]」（${organization}、${date-accessed}アクセス、[[${url}]]）"))
       ))
    (t
     (if (and (s-blank? (alist-get 'authors-full entry))
              (alist-get 'editors-full entry))
         '("${editors-full} (ed.), /[[${citekey}][${title}]]/ (${location}: ${publisher}, ${year})"
           ((ja . "${editors-full}編『[[${citekey}][${title}]]』（${publisher}、${year}）")))
       '("${authors-full}, /[[${citekey}][${title}]]/ (${location}: ${publisher}, ${year})"
         ((ja . "${authors-full}『[[${citekey}][${title}]]』（${publisher}、${year}）"))))))))

(defvar org-ok-ref-format-string
  '((article
     . ((apa . org-ok-ref-format-string--apa)
        (author-year . "[[${citekey}][${authors} ${year}]]")
        (author-year-paren . "[[${citekey}][${authors} (${year})]]")
        (full-inline . org-ok-ref-format-string--chicago-in-text)))
    (book
     . ((apa . org-ok-ref-format-string--apa)
        (author-year . "[[${citekey}][${authors} ${year}]]")
        (author-year-paren . "[[${citekey}][${authors} (${year})]]")
        (full-inline . org-ok-ref-format-string--chicago-in-text)))
    (online                   ; i.e., blog article
     . ((apa . org-ok-ref-format-string--apa)
        (author-year . "[[${citekey}][${authors} ${year}]]")
        (author-year-paren . "[[${citekey}][${authors} (${year})]]")
        (full-inline . org-ok-ref-format-string--chicago-in-text)))
    (_
     . ((apa . org-ok-ref-format-string--apa)
        (author-year . "[[${citekey}][${authors} ${year}]]")
        (author-year-paren . "[[${citekey}][${authors} (${year})]]")
        (full-inline . org-ok-ref-format-string--chicago-in-text))))
  "String formats for BibTeX entry types.")

(defun org-ok-ref-s-format-replacer (field entry)
  "Format FIELD in ENTRY with `s-format'."
  (pcase field
    ("date"
     (let ((dt (alist-get 'date entry)))
       (mulex-date-format (decoded-time-year dt)
                          (decoded-time-month dt)
                          (decoded-time-day dt)
                          (mulex-im-lang))))
    ("date-accessed"
     (let ((dt (alist-get 'urldate entry)))
       (mulex-date-format (decoded-time-year dt)
                          (decoded-time-month dt)
                          (decoded-time-day dt)
                          (mulex-im-lang))))
    (_ (alist-get (intern field) entry))))

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
                     (alist-get (intern (alist-get 'type entry))
                                org-ok-ref-format-string
                                (alist-get '_ org-ok-ref-format-string)))))
    ;; Delete the existing link
    (when-let* ((start (car range)) (end (cadr range)))
      (delete-region start end)
      (goto-char start))

    (insert
     (s-format (if (stringp fstring) fstring (funcall fstring key entry))
               #'org-ok-ref-s-format-replacer
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
