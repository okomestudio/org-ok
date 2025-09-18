;;; org-ok.el --- Org Plugin  -*- lexical-binding: t -*-
;;
;; Copyright (C) 2024-2025 Taro Sato
;;
;; Author: Taro Sato <okomestudio@gmail.com>
;; URL: https://github.com/okomestudio/org-ok
;; Version: 0.6.2
;; Keywords: org-mode, plug-in, convenience
;; Package-Requires: ((emacs "30.1") (dash "2.20") (ok "0.12.2") (org "9.7") (org-ref "3.1") (s "1.13.1"))
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
;; The `org-ok' mode is a plugin to enhance `org' in several ways. See the
;; repository README for detail.
;;
;;; Code:

(require 'org-ok-babel)
(require 'org-ok-org)
(require 'org-ok-ref)
(require 'org-ok-src)
(require 'org-ok-utils)

(cl-defun org-ok-open-at-point--ad (fun &optional arg)
  "Advise FUN (`org-open-at-point') to add enhancements.
When called with the `\\[universal-argument'] prefix ARG, the link will
be opened in the current window (default is to open in another window)."
  (pcase (car arg)
    ;; With a prefix argument, open the link in the same window
    (4 (let ((org-link-frame-setup `((file . find-file)
                                     . ,org-link-frame-setup)))
         (funcall fun arg)))
    (_ (funcall fun arg))))

;;; Minor mode

(defun org-ok-activate ()
  "Activate `org-ok-mode'."
  (advice-add #'org-open-at-point :around #'org-ok-open-at-point--ad)
  (add-hook 'org-src-mode-hook #'org-ok-src--turn-off-hl-line-mode))

(defun org-ok-deactivate ()
  "Deactivate `org-ok-mode'."
  (remove-hook 'org-src-mode-hook #'org-ok-src--turn-off-hl-line-mode)
  (advice-remove #'org-open-at-point #'org-ok-open-at-point--ad))

;;;###autoload
(define-minor-mode org-ok-mode
  "The `org-ok-mode' minor mode."
  :global nil
  :group 'org-ok-mode
  (if org-ok-mode (org-ok-activate) (org-ok-deactivate)))

(provide 'org-ok)
;;; org-ok.el ends here
