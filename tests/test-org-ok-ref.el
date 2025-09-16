;;; test-org-ok-ref.el --- Tests for org-ok-ref  -*- lexical-binding: t -*-
;;
;; Package-Requires: ((buttercup))
;;
;;; Commentary:
;;; Code:

(require 'buttercup)
(require 'org-ok-ref)

(describe
 "org-ok-ref-format-author"
 (it "format authors list correctly"
     (pcase-dolist
         (`(,src ,expected)
          '(("Doe, John and Doe, Jane" "John Doe, Jane Doe")
            ("van der Beek, Hans" "Hans van der Beek")
            ("山田, 太郎" "山田太郎")))
       (expect
        (org-ok-ref-format-author src) :to-equal expected))))

;;; test-org-ok-ref.el ends here
