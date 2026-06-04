;;; org-ok-link.el --- Org Link Plugin  -*- lexical-binding: t -*-
;;
;; Copyright (C) 2024-2026 Taro Sato
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
;;; Code:

(require 'org-element)

(defun org-ok-link-strip-description (beg end)
  "Strip descriptions from every Org link found.
If marked, only the links within region from BEG to END are edited."
  (interactive "r")
  (save-excursion
    (goto-char end)
    ;; Map backward so string mutations don't shift upcoming match positions.
    (while (re-search-backward org-link-any-re beg t)
      (let ((context (org-element-context)))
        (when (and (eq (org-element-type context) 'link)
                   (org-element-property :contents-begin context)) ; Has description
          (let* ((l-start (org-element-property :begin context))
                 (l-end (org-element-property :end context))
                 (path (org-element-property :path context))
                 (type (org-element-property :type context))
                 (post-blank (org-element-property :post-blank context))
                 (padding (if post-blank (make-string post-blank ?\s) ""))
                 (new-link (format "[[%s:%s]]" type path)))
            (delete-region l-start l-end)
            (insert new-link padding)))))))

(provide 'org-ok-link)
;;; org-ok-link.el ends here
