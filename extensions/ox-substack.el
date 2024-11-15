;;; ox-substack.el --- Substack Backend for Org Export  -*- lexical-binding: t -*-
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
;;
;; This module provides a plugin for Substack export in Org.
;;
;;; Code:

(require 'ox)

(defun ox-substack--link (link desc info)
  "Org LINK translator."
  (let* ((raw-link (org-element-property :raw-link link))
         (raw-path (org-element-property :path link))
         (type (org-element-property :type link))
         (link-is-url (member type '("http" "https" "ftp" "mailto")))
         (desc (org-string-nw-p desc)))
    (if link-is-url
        (format "<a href=\"%s\">%s</a>" raw-link (or desc raw-link))
      (if (string= (substring raw-link 0 3) "id:")
          desc
        (if (member (file-name-extension raw-link)
                    '("gif" "jpeg" "jpg" "png" "webp"))
            (format "<img src=\"%s\" />" raw-link)
          (format "<a href=\"%s\">%s</a>" raw-link desc))))))


;; TODO: It is not possible to render as HTML in a way that simple
;; copy-and-paste restore the footnote structures. For now,

(defun ox-substack--footnote-reference (footnote-reference _contents info)
  "Transcode a FOOTNOTE-REFERENCE element from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information.
See `org-html-footnote-reference'."
  (concat
   ;; Insert separator between two footnotes in a row.
   (let ((prev (org-export-get-previous-element footnote-reference info)))
     (when (org-element-type-p prev 'footnote-reference)
       (plist-get info :html-footnote-separator)))
   (let* ((n (org-export-get-footnote-number footnote-reference info))
          (label (org-element-property :label footnote-reference))
          (label (if (and (stringp label)
                          (equal label (number-to-string (string-to-number label))))
                     nil
                   label))
	        (id (format "X-footnote-anchor-%s%s" ; footnote-anchor-%s%s
		                  (or label n)
		                  (if (org-export-footnote-first-reference-p
			                     footnote-reference info)
			                    ""
                        (let ((label (org-element-property :label footnote-reference)))
                          (format
                           ".%d"
                           (org-export-get-ordinal
                            footnote-reference info '(footnote-reference)
                            `(lambda (ref _)
                               (if ,label
                                   (equal (org-element-property :label ref) ,label)
                                 (not (org-element-property :label ref)))))))))))
     (format
      "<b>[[%s]]</b>"
      (org-html--anchor
       id n (format " class=\"footref\" href=\"#X-footnote-%s\" " (or label n)) info)))))

(defun ox-substack--org-html-footnote-section (info)
  "Format the footnote section.
INFO is a plist used as a communication channel."
  (pcase (org-export-collect-footnote-definitions info)
    (`nil nil)
    (definitions
     (format
      (plist-get info :html-footnotes-section)
      (org-html--translate "Footnotes" info)
      (format
       "\n%s\n"
       (mapconcat
	      (lambda (definition)
	        (pcase definition
	          (`(,n ,label ,def)
             (when (and (stringp label)
                        (equal label (number-to-string (string-to-number label))))
               (setq label nil))
	           (let ((inline? (not (org-element-map def org-element-all-elements
				                           #'identity nil t)))
		               (anchor (org-html--anchor
                            (format "X-footnote-%s" (or label n))
			                      n
			                      (format " class=\"footnote-number\" href=\"#X-footnote-anchor-%s\" role=\"doc-backlink\"" (or label n))
			                      info))
		               (contents (org-trim (org-export-data def info))))
	             (format "<div class=\"footnote\">%s %s</div>\n"
		                   (format (plist-get info :html-footnote-format) anchor)
		                   (format "<div class=\"footnote-content\" role=\"doc-footnote\">%s</div>"
			                         (if (not inline?) contents
				                         (format "<p class=\"footpara\">%s</p>"
					                               contents))))))))
	      definitions
	      "\n"))))))

(advice-add #'org-html-footnote-section :override
            #'ox-substack--org-html-footnote-section)

(org-export-define-derived-backend
    'substack 'html
  :menu-entry
  '(?S "Export to Substack article"
       ((?o "As HTML file and open"
	          (lambda (a s v b)
	            (if a
                  (org-export-to-buffer t s v b)
                (let ((f (concat (file-name-sans-extension buffer-file-name)
                                 ".html")))
                  (org-open-file (org-export-to-file 'substack f nil s v b))))))))
  :translate-alist '((link . ox-substack--link)
                     (footnote-reference . ox-substack--footnote-reference)))

(provide 'ox-substack)
;;; ox-substack.el ends here
