;;; op-ok-babel.el --- Org Babel Plugin  -*- lexical-binding: t -*-
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

(defun op-ok-babel-run-pytest (&optional _ tangle-default)
  "Run ob-pytest using the current Python code block.
If tangle is missing, it will default to TANGLE-DEFAULT if given
or 't.py' if not given."
  (interactive)
  (let* ((params (nth 2 (org-babel-get-src-block-info)))
         (tangle (alist-get :tangle params))
         (tangle (if (string= tangle "no")
                     (or tangle-default "t.py")
                   tangle))
         (src-ob-pytest
          (concat
           (format "#+begin_src shell :var in=\"%s\" :exports none :results output\n"
                   tangle)
           "  ob-pytest \"$in\"\n"
           "#+end_src\n")))
    (if (null params)
        (message "Org Babel source block not found")
      (org-babel-tangle '(4) tangle)
      (org-forward-element)
      (insert src-ob-pytest)
      (forward-line -1)
      (let ((org-confirm-babel-evaluate nil))
        (org-ctrl-c-ctrl-c))
      (re-search-forward "#\\+RESULTS:" nil nil 1)
      (recenter-top-bottom)
      (delete-file tangle))))

(provide 'op-ok-babel)
;;; op-ok-babel.el ends here
