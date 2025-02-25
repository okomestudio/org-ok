;;; org-ok-src.el --- Org Source Plugin  -*- lexical-binding: t -*-
;;
;; Copyright (C) 2024-2025 Taro Sato
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

(require 'org-src)

(defcustom org-ok-src-noweb-ref-re "<<\\([A-Za-z0-9_.-]+\\)>>"
  "Regexp for noweb-ref."
  :type 'string
  :group 'org-plugin)

(defun org-ok-src--comment-out-noweb-ref (beg end)
  "Comment out noweb references in Org source edit buffer from BEG to END."
  (when (org-src-edit-buffer-p)
    (save-excursion
      (goto-char beg)
      (while (re-search-forward (concat "^" org-ok-src-noweb-ref-re)
                                end t 1)
        (replace-match (concat comment-start "<<\\1>>"))
        (setq end (+ end (length comment-start))))))
  end)

(defun org-ok-src--uncomment-noweb-ref (beg end)
  "Uncomment noweb references in Org source edit buffer from BEG to END."
  (when (org-src-edit-buffer-p)
    (save-excursion
      (goto-char beg)
      (while (re-search-forward (concat "^" comment-start
                                        org-ok-src-noweb-ref-re)
                                end t 1)
        (replace-match "<<\\1>>")))))

(with-eval-after-load 'org-src
  (with-eval-after-load 'reformatter
    (defun org-ok-src-reformatter--do-region-ad (func name beg end &rest rest)
      "Advise reformatter--do-region to ignore noweb-refs."
      (let ((end (org-ok-src--comment-out-noweb-ref beg end)))
        (apply func `(,name ,beg ,end ,@rest))
        (org-ok-src--uncomment-noweb-ref beg end)))

    (advice-add #'reformatter--do-region :around
                'org-ok-src-reformatter--do-region-ad)))

(provide 'org-ok-src)
;;; org-ok-src.el ends here
