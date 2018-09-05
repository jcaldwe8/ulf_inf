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

;; Helper function to assert that two lists are equal, ignoring order and
;; repetitions.
(defun set-assert-equal (actual expect)
  (let ((unique-actual (remove-duplicates actual :test #'equal))
        (unique-expect (remove-duplicates expect :test #'equal))
        shared expect-only actual-only
        reordered-expect reordered-actual)
    ;; Construct a reordered version of each list such that elements that
    ;; are shared come first in both.
    (setq shared (intersection unique-expect unique-actual :test #'equal))
    (setq expect-only (set-difference unique-expect unique-actual
                                      :test #'equal))
    (setq actual-only (set-difference unique-actual unique-expect 
                                      :test #'equal))
    (setq reordered-expect (append shared expect-only))
    (setq reordered-actual (append shared actual-only))
    ;; Assert equal length.
    (assert-equal (length reordered-expect) (length reordered-actual)
                  (length reordered-expect) (length reordered-actual)
                  reordered-expect reordered-actual)
    ;; Assert that each element at a time.
    (mapcar #'(lambda (x)
                (let ((expect-elem (first x))
                      (actual-elem (second x)))
                  (assert-equal expect-elem actual-elem
                                expect-elem actual-elem
                                reordered-expect reordered-actual)))
            (mapcar #'list reordered-expect reordered-actual))))
  

