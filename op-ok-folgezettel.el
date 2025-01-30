;;; op-ok-folgezettel.el --- Org Plugin for Folgezettel  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun op-ok-folgezettel--title-pos ()
  (save-excursion
    (beginning-of-buffer)
    (re-search-forward "#\\+title: ")
    `(,(point) ,(org-id-get))))

(defun op-ok-folgezettel-show ()
  (interactive)
  (setq-local op-ok-folgezettel--ovs nil)
  (let* ((item (op-ok-folgezettel--title-pos))
         (pos (car item))
         (oid (cadr item))
         (oid (if (eq (length oid) 36) (substring oid 0 6) oid))
         (ov (make-overlay pos pos)))
    (overlay-put ov 'before-string (format "%s " oid))
    (push ov op-ok-folgezettel--ovs))

  (org-element-map (org-element-parse-buffer) 'link
    (lambda (link)
      (when (string= (org-element-property :type link) "id")
        (let* ((oid (org-element-property :path link))
               (pos (org-element-property :begin link))
               (ov (make-overlay pos pos)))
          (overlay-put ov 'before-string (format "%s " oid))
          (push ov op-ok-folgezettel--ovs))))))

(defun op-ok-folgezettel-hide ()
  (interactive)
  (dolist (ov op-ok-folgezettel--ovs)
    (delete-overlay ov)))

(provide 'op-ok-folgezettel)
;;; op-ok-folgezettel.el ends here
