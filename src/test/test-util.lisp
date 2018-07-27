;;; Gene Kim 7-27-2018
;;; Utility functions for testing.

;; Helper function to assert equality between lists, one by one.
;; TODO: make the exact assert function an argument for generalizability.
(defun list-assert-equal (actual expect)
  (assert-equal (length expect) (length actual) 
                (length expect) (length actual) 
                expect actual)
  (mapcar #'(lambda (x) 
              (let ((expect (first x))
                    (actual (second x)))
                (assert-equal expect actual 
                              expect actual)))
          (mapcar #'list expect actual)))

;; TODO: write set-assert-equal which is an order agnostic variant of
;;       list-assert-equal.

