;; Jordan Caldwell 10-17-2018
;;
;; Unit tests for factives


;; Applies implicative rules to ulf depending on polarity context
(defun apply-implicative-rules (ulf)
  (ttt:apply-rules
   (mapcar (lambda (x) (if (equalp (slot-value x 'polarity)
				   (get-implicative-polarity ulf))
			   (slot-value x 'rule)))
	   *infer-from-implicative-rules*)
   (remove-aux-not ulf) :shallow t))

;; Gets segments of implicative ulf and returns polarity
(defun get-implicative-polarity (ulf)
  (let ((seg (get-ulf-segments-vp ulf)))
    (get-segment-polarity (first seg) (second seg) (third seg))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Basic Factive Inferences ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Type A: Certain predicates with that-clause subjects
;;;         that S be odd/tragic
;;;         that S count/matter/suffice
;;;         NOT relating to probability of the event; e.g. likely, happen

;; It's odd that I was hungry after lunch
;; -> I was hungry after lunch
;;

(define-test type-a-factives-inferences-tests
    "Testing functionality of type A factives inferences"
  (:tag :typeA :factive-inferences)
  (assert-equal
   '(I.pro (((past be.v) hungry.a)
	    (adv-e (after.p ({ref}.d lunch.n)))))
   (apply-implicative-rules
    '(It-extra.pro
      ((pres be.v) (odd.a
                    (that (I.pro
	                   (((past be.v) hungry.a)
		            (adv-e (after.p ({ref}.d lunch.n)))))))))))
  (assert-equal
   'nil
   (apply-implicative-rules
    ((It-extra.pro
      ((pres be.v) (unlikely.a
                    (that ((the.d fruit.n)
                           ((pres be.v) spoiled.a)))))))))
  ;;add more here
  )

;;; Type B: Certain emotive adjectives with complements:
;;;         NP be happy/glad/furious that S
;;;         NP be sad/delighted/disappointed to VP
;;;         NOT relating to probability of the event; e.g. hopeful, willing

;; The CEO is furious that an employee was absent
;; -> an employee was absent
;;

(define-test type-b-factives-inferences-tests
    "Testing functionality of type B factives inferences"
  (:tag :typeB :factive-inferences)
  (assert-equal
   '((an.d employee.n) ((past be.v) absent.a))
   (apply-implicative-rules
    '((the.d CEO.n)
      ((pres be.v) (furious.a
		    (that ((an.d employee.n)
			   ((past be.v) absent.a))))))))
  (assert-equal
   'nil
   (apply-implicative-rules
    '((the.d janitor.n) ((pres be.v)
                         (willing.a
                          (to (wash.v (the.d chalkboard.n))))))))
  )

;;; Type C: Certain propositional attitude verbs:
;;;         NP know/regret/forget/remember that S
;;;         NOT relating to probability of the event; e.g. believe, think

;; Eric misremembered not completing his homework, but did not regret it
;; -> possibly no implication
;; It depends on whether it is proper to 'know' something that is false in the real world, but true in a different world

(define-test type-c-factives-inferences-tests
    "Testing functionality of type C factives inferences"
  (:tag :typeC :factive-inferences)
  (assert-equal
   'nil
   (apply-implicative-rules
    '((| Eric|
       ((past misremember.v)
	(not ((prog complete.v) (his.d homework.n)))))
      (but.cc
       ({he}.pro ((past do.aux-s) not (regret.v it.pro)))))))
  (assert-equal
   'nil
   (apply-implicative-rules
    '(I.pro ((past dream.v)
             (that ((I.pro ((past be.v) |German|.a)) and.cc
                    (nobody.pro (know.v it.pro))))))))
  (assert-equal
   'nil
   (apply-implicative-rules
    '(We.pro (((past mishear.v)
              (that ((the.d dinner.n) ((past be.v) free.a)))
              and.cc
               (be.v (glad.a (to not (have.v (to pay.v))))))))))
  (assert-equal
   'nil
   (apply-implicative-rules
    '(| Sally| (((past misremember.v) not ((perf leave.v) (a.d tip.n)))
                and.cc
                (regret.v it.pro)))))
  (assert-equal
  '(| Sally| (((past perf) not leave.v) (a.d tip.n)))
  (apply-implicative-rules
   '(| Sally| ((past regret.v) not ((perf leave.v) (a.d tip.n))))))
  )

;;; Type D: Verbs of discovery:
;;;         NP discover/find out/notice/observe that S
;;;         NP discovered/found out/noticed/observed to VP
;;;         NOT relating to probability of the event; e.g. suspect/suspected

;; The professors found out that the coffee machine was broken
;;-> the coffee machine was broken
;;
;; The professors did not find out that the coffee machine was broken
;; -> no implication without context
;;
;; Did the professors find out that the coffee machine was broken?
;; -> no implication without context
;;
;; If the professors find out that the coffee machine was broken, it should have been fixed
;; -> no implication without context
;;

(define-test type-d-factives-inferences-tests
    "Testing functionality of type D factives inferences"
  (:tag :typeD :factive-inferences)
  (assert-equal
   '((the.d (coffee.a machine.n))
     ((past be.v) broken.a))
   (apply-implicative-rules
    '((the.d (plur professor.n))
      ((past find-out.v)
       (that ((the.d (coffee.n machine.n))
	((past be.v) broken.a)))))))
  (assert-equal
   'nil
   (apply-implicative-rules
    '((the.d (plur professor.n))
      ((past do.aux-s) not
       (find-out.v (that
		    ((the.d (coffee.n machine.n))
		     ((past be.v) broken.a))))))))
  (assert-equal
   'nil
   (apply-implicative-rules
    '((past do.aux-s)
      ((the.d (plur professor.n))
       (find-out.v (that
                    ((the.d (coffee.n machine.n))
                     (past be.v) broken.a)))) ? )))
  (assert-equal
   'nil
   (apply-implicative-rules
    '((if.ps ((the.d (plur professor.n))
              ((past find-out.v)
               (that ((the.d (coffee.n machine.n))
                      (past be.v) broken.a)))))
      (it.pro ((cf should.aux-s) (perf fix.v))))))
  (assert-equal
   'nil
   (apply-implicative-rules
    '((the.d police.n) ((past do.aux-s) not
                        (discover.v
                         (that ((any.d (plur car.n))
                                (((past perf) tamper.v) with.a))))))))
  (assert-equal
   'nil
   (apply-implicative-rules
    '((if.ps ((the.d police.n)
              ((past do.aux-s) not
               (discover.v
                (that ((any.d (plur car.n))
                       (((past perf) tamper.v)) with.a))))))
      ((the.d (plur suspect.n)) ((cf should.aux-s) (be.v (let.v go.a)))))))
  (assert-equal
   'nil
   (apply-implicative-rules
    '((Perhaps.adv-s
       ((the.d police.n)
        ((pres will.aux-s) discover.v
         (that ((some.d (plur car.n))
                (((past perf) tamper.v) with.a))))))
      but.cc
      ((so.adv-e far.a) they.pro ((pres have.v) not)))))
  )

;;; Type E: Certain verbs of communication: acknowledge, admit, confess; as opposed to say 

;; Tim did not admit that he didn't study for the exam
;; -> no implication without context
;;

(define-test type-e-factives-inferences-tests
    "Testing functionality of type E factives inferences"
  (:tag :typeE :factive-inferences)
  (assert-equal
   'nil
   (apply-implicative-rules
    '(| Tim|
      ((pres do.aux-s) not
       (admit.v
	(that (he.pro
	       ((pres do.aux-s)
		not (study.v (for.p (the.d exam.n)))))))))))
  (assert-equal
   'nil
   (apply-implicative-rules
    '(| Cheney| ((past do.aux-s) not
                 (acknowledge.v
                  (that ((the.d (| Iraq|.a war.n)) ((past be.v)
                                                    (= (a.d mistake.n))))))))))
  )

