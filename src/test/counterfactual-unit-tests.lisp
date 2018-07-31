;; Gene Kim 7-30-2018
;;
;; Unit tests for functions in counterfactual-inferences.lisp.




;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests for conditional 'were to' preprocessing
;;;;;;;;;;;;;;;;;;;;;;;;;

;; Define here in case we modify 'counterfactual-preprocess' in ways that
;; don't follow assumptions made in this test.
(defun run-were-to-preproc (ulf)
  (flatten-adv-s   
    (ttt:apply-rule *preprocess-if-were-to*
                    (ttt:apply-rule *uninvert-if-were-to-inv* ulf))))

;````````````````````````````````````
; If I were to go to sleep -> If I went to sleep
; If I were ever to go to sleep -> If I ever went to sleep
; If I were to not go to sleep -> If I did not go to sleep
; If I were not to go to sleep -> If I did not go to sleep
; If I were to not be a person -> If I were not a person
(define-test basic-were-to-preproc-test
  "Tests conditional 'were to' handling on basic cases using the TTT rules directly."
  (:tag :cf-were-to-preproc :basic :counterfactual-inferences)
  (let (
        ;; Actions.
        (gotosleep 
          '((If.ps (i.pro ((cf were.v) (to (go.v (to.p-arg (k sleep.n)))))))
            (I.pro ((cf will.aux-s) (get.v (some.d rest.n))))))
        (evergotosleep
          '((If.ps (i.pro ((cf were.v) ever.adv-s (to (go.v (to.p-arg (k sleep.n)))))))
            (I.pro ((cf will.aux-s) (get.v (some.d rest.n))))))
        (notgotosleep 
          '((If.ps (i.pro ((cf were.v) (to not.adv-s (go.v (to.p-arg (k sleep.n)))))))
            (I.pro ((cf will.aux-s) (get.v (some.d rest.n))))))
        (notgotosleep2
          '((If.ps (i.pro ((cf were.v) not.adv-s (to (go.v (to.p-arg (k sleep.n)))))))
            (I.pro ((cf will.aux-s) (get.v (some.d rest.n))))))
        ;; Statives.
        (beaperson
          '((If.ps (I.pro ((cf were.v) (to (be.v (= (a.d person.n)))))))
            (I.pro ((cf will.aux-s) (be.v (able.a (to talk.v)))))))
        (everbeaperson
          '((If.ps (I.pro ((cf were.v) ever.adv-s (to (be.v (= (a.d person.n)))))))
            (I.pro ((cf will.aux-s) (be.v (able.a (to talk.v)))))))
        (notbeaperson
          '((If.ps (I.pro ((cf were.v) (to not.adv-s (be.v (= (a.d person.n)))))))
            (I.pro ((cf will.aux-s) (be.v (able.a (to talk.v)))))))
        (notbeaperson2
          '((If.ps (I.pro ((cf were.v) not.adv-s (to (be.v (= (a.d person.n)))))))
            (I.pro ((cf will.aux-s) (be.v (able.a (to talk.v))))))))
    ;; Actual tests.
    (assert-equal 
      '((If.ps (i.pro ((cf go.v) (to.p-arg (k sleep.n)))))
        (I.pro ((cf will.aux-s) (get.v (some.d rest.n)))))
      (run-were-to-preproc gotosleep))
    (assert-equal 
      '((If.ps (i.pro ever.adv-s ((cf go.v) (to.p-arg (k sleep.n)))))
        (I.pro ((cf will.aux-s) (get.v (some.d rest.n)))))
      (run-were-to-preproc evergotosleep))
    (assert-equal
      '((If.ps (i.pro ((cf do.aux-s) not.adv-s (go.v (to.p-arg (k sleep.n))))))
        (I.pro ((Cf will.aux-s) (get.v (some.d rest.n)))))
      (run-were-to-preproc notgotosleep))
    (assert-equal
      '((If.ps (i.pro ((cf do.aux-s) not.adv-s (go.v (to.p-arg (k sleep.n))))))
        (I.pro ((Cf will.aux-s) (get.v (some.d rest.n)))))
      (run-were-to-preproc notgotosleep2))

    (assert-equal
      '((If.ps (I.pro ((cf be.v) (= (a.d person.n)))))
        (I.pro ((cf will.aux-s) (be.v (able.a (to talk.v))))))
      (run-were-to-preproc beaperson))
    (assert-equal
      '((If.ps (I.pro ((cf be.v) ever.adv-s (= (a.d person.n)))))
        (I.pro ((cf will.aux-s) (be.v (able.a (to talk.v))))))
      (run-were-to-preproc everbeaperson))
    (assert-equal
      '((If.ps (I.pro ((cf be.v) not.adv-s (= (a.d person.n)))))
        (I.pro ((cf will.aux-s) (be.v (able.a (to talk.v))))))
      (run-were-to-preproc notbeaperson))
    (assert-equal
      '((If.ps (I.pro ((cf be.v) not.adv-s (= (a.d person.n)))))
        (I.pro ((cf will.aux-s) (be.v (able.a (to talk.v))))))
      (run-were-to-preproc notbeaperson2))))

(define-test inv-were-to-preproc-test
  "Tests conditional 'were to' handling on inverted cases using the TTT rules directly."
  (:tag :cf-were-to-preproc :inv :counterfactual-inferences)
  (let (
        ;; Actions.
        (gotosleep 
          '(((cf were.v) i.pro (to (go.v (to.p-arg (k sleep.n)))))
            (I.pro ((cf will.aux-s) (get.v (some.d rest.n))))))
        (evergotosleep
          '(((cf were.v) i.pro ever.adv-s (to (go.v (to.p-arg (k sleep.n)))))
            (I.pro ((cf will.aux-s) (get.v (some.d rest.n))))))
        (notgotosleep 
          '(((cf were.v) i.pro (to not.adv-s (go.v (to.p-arg (k sleep.n)))))
            (I.pro ((cf will.aux-s) (get.v (some.d rest.n))))))
        (notgotosleep2
          '(((cf were.v) i.pro not.adv-s (to (go.v (to.p-arg (k sleep.n)))))
            (I.pro ((cf will.aux-s) (get.v (some.d rest.n))))))
        ;; Statives.
        (beaperson
          '(((cf were.v) I.pro (to (be.v (= (a.d person.n)))))
            (I.pro ((cf will.aux-s) (be.v (able.a (to talk.v)))))))
        (everbeaperson
          '(((cf were.v) I.pro ever.adv-s (to (be.v (= (a.d person.n)))))
            (I.pro ((cf will.aux-s) (be.v (able.a (to talk.v)))))))
        (notbeaperson
          '(((cf were.v) I.pro (to not.adv-s (be.v (= (a.d person.n)))))
            (I.pro ((cf will.aux-s) (be.v (able.a (to talk.v)))))))
        (notbeaperson2
          '(((cf were.v) I.pro not.adv-s (to (be.v (= (a.d person.n)))))
            (I.pro ((cf will.aux-s) (be.v (able.a (to talk.v))))))))

    ;; Actual tests.
    (assert-equal 
      '((If.ps (i.pro ((cf go.v) (to.p-arg (k sleep.n)))))
        (I.pro ((cf will.aux-s) (get.v (some.d rest.n)))))
      (run-were-to-preproc gotosleep))
    (assert-equal 
      '((If.ps (i.pro ever.adv-s ((cf go.v) (to.p-arg (k sleep.n)))))
        (I.pro ((cf will.aux-s) (get.v (some.d rest.n)))))
      (run-were-to-preproc evergotosleep))
    (assert-equal
      '((If.ps (i.pro ((cf do.aux-s) not.adv-s (go.v (to.p-arg (k sleep.n))))))
        (I.pro ((Cf will.aux-s) (get.v (some.d rest.n)))))
      (run-were-to-preproc notgotosleep))
    (assert-equal
      '((If.ps (i.pro ((cf do.aux-s) not.adv-s (go.v (to.p-arg (k sleep.n))))))
        (I.pro ((Cf will.aux-s) (get.v (some.d rest.n)))))
      (run-were-to-preproc notgotosleep2))

    (assert-equal
      '((If.ps (I.pro ((cf be.v) (= (a.d person.n)))))
        (I.pro ((cf will.aux-s) (be.v (able.a (to talk.v))))))
      (run-were-to-preproc beaperson))
    (assert-equal
      '((If.ps (I.pro ((cf be.v) ever.adv-s (= (a.d person.n)))))
        (I.pro ((cf will.aux-s) (be.v (able.a (to talk.v))))))
      (run-were-to-preproc everbeaperson))
    (assert-equal
      '((If.ps (I.pro ((cf be.v) not.adv-s (= (a.d person.n)))))
        (I.pro ((cf will.aux-s) (be.v (able.a (to talk.v))))))
      (run-were-to-preproc notbeaperson2))))

(define-test were-to-preproc-edge-cases
  "Tests conditional 'were to' handling on edge cases using the TTT rules directly."
  (:tag :cf-were-to-preproc :edge-cases :counterfactual-inferences)
  (let ((noifwereto
          ;; They are to go home today.
          '(They.pro ((pres be.v) (to ((go.v (k home.n)) today.adv-s)))))
        (noifwereto2
          ;; (mis annotated) They were to go home today.
          '(They.pro ((cf were.v) (to ((go.v (k home.n)) today.adv-s)))))
        (noweretoif
          '((If.ps (I.pro ((cf were.v) (at.p (k home.n)))))
            (I.pro ((cf will.aux-s) (be.v (able.a (to help.v))))))))
    ;; Since these structures don' satisfy the constraints of if-were-to
    ;; handling, nothing should happen.
    (assert-equal
      noifwereto
      (run-were-to-preproc noifwereto))
    (assert-equal
      noifwereto2
      (run-were-to-preproc noifwereto2))
    (assert-equal
      noweretoif
      (run-were-to-preproc noweretoif))))

