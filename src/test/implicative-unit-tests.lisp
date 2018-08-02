;;; Ben Kane 7-31-2018
;;;
;;; Tests for implicative-inferences.lisp and implicative-weak-inferences.lisp

;; Applies implicative rules to ulf depending on polarity context
(defun apply-implicative-rules (ulf)
  (ttt:apply-rules
    (mapcar (lambda (x) (if (equalp (slot-value x 'polarity)
    ;; Implement dynamic polarity below...
    '+
    ) (slot-value x 'rule))) *infer-from-implicative-rules*) ulf :shallow t))

(setq ulf1 '((PRES KNOW.V) (THAT (|MARY| ((PAST BE.V) COLD.A)))))
(setq ulf2 '((THE.D MAN.N) ((PRES KNOW.V) (THAT (|MARY| ((PAST BE.V) COLD.A))))))

(define-test basic-implicative-inferences-tests
  "Testing basic functionality of implicative inferences"
  (:tag :basic :implicative-inferences)
  (assert-equal
    '(|Mary| (was.v cold.a))
      (apply-implicative-rules
    '(|Jack| (accept.v (that (|Mary| (was.v cold.a)))))))
  ;; Define more here...
  )