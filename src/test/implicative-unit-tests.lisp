;;; Ben Kane 7-31-2018
;;;
;;; Tests for implicative-inferences.lisp and implicative-weak-inferences.lisp



;; Applies implicative rules to ulf depending on polarity context
(defun apply-implicative-rules (ulf)
  (ttt:apply-rules
    (mapcar (lambda (x) (if (equalp (slot-value x 'polarity)
      (let ((seg (get-ulf-segments-vp ulf)))
        (get-segment-polarity (first seg) (second seg) (third seg))))
    (slot-value x 'rule))) *infer-from-implicative-rules*) (remove-aux-not ulf) :shallow t))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Basic implicative inferences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; The man knows that Mary was cold -> Mary was cold
; 
(define-test basic-implicative-inferences-tests
  "Testing basic functionality of implicative inferences"
  (:tag :basic :implicative-inferences)
  (assert-equal
    '(| Mary| ((past be.v) cold.a))
      (apply-implicative-rules
    '((the.d man.n) ((pres know.v) (that (| Mary| ((past be.v) cold.a)))))))
  (assert-equal
    '(| Mary| ((past be.v) cold.a))
      (apply-implicative-rules
    '((the.d man.n) ((past do.aux-s) not (know.v (that (| Mary| ((past be.v) cold.a))))))))
  ;; Define more here...
  )