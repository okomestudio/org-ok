;;; org-plugin-ok.el --- Org Plugin  -*- lexical-binding: t -*-
;;
;; Copyright (C) 2024 Taro Sato
;;
;; Author: Taro Sato <okomestudio@gmail.com>
;; URL: https://github.com/okomestudio/org-roam-plugin-ok
;; Version: 0.1
;; Keywords: org-mode, plug-in
;; Package-Requires: ((emacs "29.1") (org "9.7"))
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
;; The `org-plugin-ok' mode is a plugin to enhance `org' in
;; several ways. See the repository README for detail.
;;
;;; Code:

(defun org-plugin-ok-activate ()
  "Activate `org-plugin-ok-mode'."
  (require 'op-ok-src))

(defun org-plugin-ok-deactivate ()
  "Deactivate `org-plugin-ok-mode'."
  nil)

;;;###autoload
(define-minor-mode org-plugin-ok-mode
  "The `org-plugin-ok-mode' minor mode."
  :global nil
  :group 'org-plugin-ok-mode
  (if org-plugin-ok-mode
      (org-plugin-ok-activate)
    (org-plugin-ok-deactivate)))

(provide 'org-plugin-ok)
;;; org-plugin-ok.el ends here
