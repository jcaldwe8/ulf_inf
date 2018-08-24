;;; Ben Kane 7-31-2018
;;;
;;; Tests for implicative-inferences.lisp and implicative-weak-inferences.lisp



;; Applies implicative rules to ulf depending on polarity context
(defun apply-implicative-rules (ulf)
  (ttt:apply-rules
    (mapcar (lambda (x) (if (equalp (slot-value x 'polarity)
      (get-implicative-polarity ulf))
    (slot-value x 'rule))) *infer-from-implicative-rules*) (remove-aux-not ulf) :shallow t))

;; Gets segments of implicative ulf and returns polarity
(defun get-implicative-polarity (ulf)
  (let ((seg (get-ulf-segments-vp ulf)))
    (get-segment-polarity (first seg) (second seg) (third seg))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Basic implicative inferences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; The man knows that Mary was cold -> Mary was cold
; 
(define-test basic-implicative-inferences-tests-pol-pos
  "Testing basic functionality of implicative inferences"
  (:tag :basic :implicative-inferences :pol-pos)
  (assert-equal
    '(| Mary| ((past be.v) cold.a))
      (apply-implicative-rules
    '((the.d man.n) ((pres know.v) (that (| Mary| ((past be.v) cold.a)))))))
  (assert-equal
    '(| Mary| ((past be.v) cold.a))
      (apply-implicative-rules
    '((the.d man.n) ((past do.aux-s) not (know.v (that (| Mary| ((past be.v) cold.a))))))))
  (assert-equal
    '(| Mary| ((past be.v) cold.a))
      (apply-implicative-rules
    '(| Jack| (accept.v (that (| Mary| ((past be.v) cold.a)))))))
  (assert-equal
    '(| John| steal.v)
      (apply-implicative-rules
    '(| John| ((past confess.v) (ka steal.v)))))
  (assert-equal
    '(| Mary| (can.v swim.v))
      (apply-implicative-rules
    '(| John| ((past allow.v) | Mary| (ka swim.v)))))
  (assert-equal
    '((the.d man.n) (can.v swim.v))
      (apply-implicative-rules
    '(| John| ((past allow.v) ((the.d man.n) (ka swim.v))))))
  (assert-equal
    '((the.d program.n) is-a (ka (be.v (a.d fraud.n))))
      (apply-implicative-rules
    '((the.d program.n) ((past turn-out.v) (ka (be.v (a.d fraud.n)))))))
  (assert-equal
    '(| John| ((past be.v) drunk.a))
      (apply-implicative-rules
    '((that (| John| ((past be.v) drunk.a))) (pasv ascertain.v))))
  (assert-equal
    '(| John| ((past be.v) not drunk.a))
      (apply-implicative-rules
    '((that (| John| ((past be.v) not drunk.a))) (pasv ascertain.v))))
  (assert-equal
    '(probably (he.pro ((past kill.v) | Tom|)))
      (apply-implicative-rules
    '(| John| ((past agree.v) (that (he.pro ((past kill.v) | Tom|)))))))
  (assert-equal
    '(not (he.pro ((past be.v) over.p eighteen.n)))
      (apply-implicative-rules
    '(| John| ((past lie.v) (that (he.pro ((past be.v) over.p eighteen.n)))))))
  (assert-equal
    '((his.d friend.n) ((past betray.v) him.pro))
      (apply-implicative-rules
    '((that ((his.d friend.n) ((past betray.v) him.pro))) ((past shock.v) | John|))))
  (assert-equal
    '(she.pro ((past leave.v) (a.d mess.n)))
      (apply-implicative-rules
    '(| John| ((past scold.v) | Mary| (that (she.pro ((past leave.v) (a.d mess.n))))))))
  (assert-equal
    '(she.pro ((past leave.v) (a.d mess.n)))
      (apply-implicative-rules
    '(| John| ((past scold.v) (| Mary| (that (she.pro ((past leave.v) (a.d mess.n)))))))))
  (assert-equal
    '(| John| (take.v (piano.a lesson.n)))
      (apply-implicative-rules
    '(| John| ((pres continue.v) (ka (take.v (piano.a lesson.n)))))))
  (assert-equal
    '(| John| (clean.v (the.d house.n)))
      (apply-implicative-rules
    '(| John| ((pasv force.v) (ka (clean.v (the.d house.n)))))))
  (assert-equal
    '(| John| ((past mean.v) (ka (harm.v | Mary|))))
      (apply-implicative-rules
    '(| John| ((past mean.v) (ka (harm.v | Mary|))))))
  (assert-equal
    '(| John| (write.v | Epilog|))
      (apply-implicative-rules
    '(| John| ((past use.v) | Lisp| (ka (write.v | Epilog|))))))
  (assert-equal
    '(| John| (write.v | Epilog|))
      (apply-implicative-rules
    '(| John| ((past use.v) (| Lisp| (ka (write.v | Epilog|)))))))
  ;; Define more here...
  ;; (assert-equal
  ;;   '()
  ;;     (apply-implicative-rules
  ;;   '()))
  )

  (define-test basic-implicative-inferences-tests-pol-neg
  "Testing basic functionality of implicative inferences"
  (:tag :basic :implicative-inferences :pol-neg)
  (assert-equal
    '(| Mary| ((past be.v) cold.a))
      (apply-implicative-rules
    '((the.d man.n) ((past do.aux-s) not (know.v (that (| Mary| ((past be.v) cold.a))))))))
  (assert-equal
    '(| John| (harm.v | Mary|))
      (apply-implicative-rules
    '(| John| ((past do.aux-s) not (mean.v (ka (harm.v | Mary|)))))))
  ;; Define more here...
  ;; (assert-equal
  ;;   '()
  ;;     (apply-implicative-rules
  ;;   '()))
  )