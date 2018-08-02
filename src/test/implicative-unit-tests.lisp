;;; Ben Kane 7-31-2018
;;;
;;; Tests for implicative-inferences.lisp and implicative-weak-inferences.lisp



;; Applies implicative rules to ulf depending on polarity context
(defun apply-implicative-rules (ulf)
  (ttt:apply-rules
    (mapcar (lambda (x) (if (equalp (slot-value x 'polarity)
      (let ((seg (segment-ulf ulf)))
        (get-segment-polarity (first seg) (second seg) (third seg))))
    (slot-value x 'rule))) *infer-from-implicative-rules*) ulf :shallow t))

;; Function to get ulf segments necessary for dynamic polarity
;; NOTE: needs to be implemented (currently using static example which returns positive polarity)
(defun segment-ulf (ulf)
  (let*
    ((ulfp1 '((PRES KNOW.V) (THAT (|MARY| ((PAST BE.V) COLD.A)))))
    (ulfp2 '(THE.D MAN.N))
    (compulf (list ulfp2 ulfp1)))
  ;; (segment parent fullulf)
  (list ulfp1 compulf compulf)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Basic implicative inferences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; The man knows that Mary was cold -> Mary was cold
; 
(define-test basic-implicative-inferences-tests
  "Testing basic functionality of implicative inferences"
  (:tag :basic :implicative-inferences)
  (assert-equal
    '(|Mary| ((past be.v) cold.a))
      (apply-implicative-rules
    '((the.d man.n) ((pres know.v) (that (|Mary| ((past be.v) cold.a)))))))
  ;; Define more here...
  )