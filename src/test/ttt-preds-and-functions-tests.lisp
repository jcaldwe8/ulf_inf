;;; Gene Kim 7-30-2018
;;;
;;; Tests for ttt-preds-and-functions.lisp.

;;;;;;;;;;;;;;;;;;;;;
;;; remove-were-to!
;;;;;;;;;;;;;;;;;;;;;

; If I were to [go to sleep] -> If I [went to sleep]
; If I were ever to [go to sleep] -> If I ever [went to sleep]
; If I were to [not go to sleep] -> If I [did not go to sleep]
; If I were to [not be a person] -> If I [were not a person]
; If I were to [not be running] -> If I [were not running]
; If I were to [have not run] -> If I [hadn't run]
(define-test basic-remove-were-to
  "Test remove-were-to! on basic cases."
  (:tag :ttt-preds-and-functions :remove-were-to)
  (let ((gotosleep      '((go.v (adv-a (to.p (k sleep.n))))))
        (notgotosleep   '(not.adv-s (go.v (adv-a (to.p (k sleep.n))))))
        (inotbeaperson  '(not.adv-s (be.v (= (a.d person.n)))))
        (beaperson      '((be.v (= (a.d person.n)))))
        (notberunning   '(not.adv-s (prog run.v)))
        (havenotrun     '((perf not.adv-s run.v))))

    (assert-equal '((cf go.v) (adv-a (to.p (k sleep.n))))
                  (remove-were-to! gotosleep))
    (assert-equal '((cf do.aux-s) not.adv-s (go.v (adv-a (to.p (k sleep.n)))))
                  (remove-were-to! notgotosleep))
    (assert-equal '((cf be.v) not.adv-s (= (a.d person.n)))
                  (remove-were-to! notbeaperson))
    (assert-equal '((cf prog) not.adv-s run.v)
                  (remove-were-to! notberunning))
    (assert-equal '((cf perf) not.adv-s run.v)
                  (remove-were-to! havenotrun))))

(define-test edge-cases-remove-were-to
  "Test remove-were-to! on edge cases."
  (:tag :ttt-preds-and-functions :remove-were-to)
  ;; Atom.
  (assert-equal nil (remove-were-to! 'run.v))
  (assert-equal nil (remove-were-to! nil))
  (assert-equal nil (remove-were-to! 1)))


;;;;;;;;;;;;;;;;;;;;;
;;; negate-vp!
;;;;;;;;;;;;;;;;;;;;;

;; TODO

;;;;;;;;;;;;;;;;;;;;;
;;; non-cf-version!
;;;;;;;;;;;;;;;;;;;;;

;; TODO

