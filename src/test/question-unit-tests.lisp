;;; Gene Kim 10-26-2018
;;;
;;; Unite tests for functinos in question-inferences.lisp.


;``````````````````````````````````````
; Where did John go? -> You probably know where John went
;                    -> I probably don't know where John went
; When are you getting married?
;   -> You probably know when you are getting married
;   -> I probably don't know when you are getting married
; Did that happen? -> You probably know whether that happened
;                  -> I probably don't know whether that happened.
(define-test question-act-test
  "Tests for question act inferences"
  (:tag :q-act)
  (let (;; Where did John go
        (f1 '((sub where.pq ((past do.aux-s) |John| (go.v *h))) ?))
        ;; When are you getting married?
        (f2 '((sub when.pq ((pres prog) you.pro ((get.v married.a) *h))) ?))
        ;; Did that happen?
        (f3 '(((past do.aux-s) that.pro happen.v) ?)))
    (assert-equal
      '(you.pro probably.adv-s ((pres know.v) (ans-to (sub where.pq (|John| ((past go.v) *H))))))
      (run-you-know-q-act f1))
    (assert-equal
      '(I.pro probably.adv-s ((pres do.aux-s) not (know.v (ans-to (sub where.pq (|John| ((past go.v) *h)))))))
      (run-i-not-know-q-act f1))

    (assert-equal
      '(you.pro probably.adv-s ((pres know.v) (ans-to (sub when.pq (you.pro ((pres prog) ((get.v married.a) *h)))))))
      (run-you-know-q-act f2))
    (assert-equal
      '(i.pro probably.adv-s ((pres do.aux-s) not (know.v (ans-to (sub when.pq (you.pro ((pres prog) ((get.v married.a) *h))))))))
      (run-i-not-know-q-act f2))

    (assert-equal
      '(you.pro probably.adv-s ((pres know.v) (whether (that.pro (past happen.v)))))
      (run-you-know-q-act f3))
    (assert-equal
      '(i.pro probably.adv-s ((pres do.aux-s) not (know.v (whether (that.pro (past happen.v))))))
      (run-you-know-q-act f3))))




