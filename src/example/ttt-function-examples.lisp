;;; Gene 7-25-2018
;;;
;;; This file is meant to show some minimal and interesting examples of the TTT
;;; function mapping rules.

(defmacro run-ex (body)
  `(format t "Running example ~s~%Result ~s~%~%" (quote ,body) ,body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Basic example 'make-scary!'
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Define the function.
;;  A -> ABOO
;;  B -> BBOO
;;  AMAZING -> AMAZINGBOO
;;  (A B C) -> (ABOO BBOO CBOO)
(defun make-scary! (x)
  (cond
    ((null x) x)
    ((symbolp x) (intern 
                   (append (coerce (string x) 'list)
                           '(#\B #\O #\O))))
    ((atom x) x)
    (t (mapcar #'make-scary! x))))
;; Define some lists.
(setq lst1 '(1 2 3))
(setq lst2 '(a b c))
(setq lst3 '(a (b a) c ((a) d)))
;; Define some rules that use the function.
(setq all-scary-rule
      '(/ _! (make-scary! _!)))
(setq scary-a
      '(/ a (make-scary! a)))
;; This set of examples show using the ttt mapping function 'make-scary!'.
(defun run-example-set1 ()
  (run-ex (ttt:apply-rule all-scary-rule lst1 :max-n 1))
  (run-ex (ttt:apply-rule all-scary-rule lst2 :max-n 1))
  (run-ex (ttt:apply-rule all-scary-rule lst3 :max-n 1))

  (run-ex (ttt:apply-rule scary-a lst1))
  (run-ex (ttt:apply-rule scary-a lst2))
  (run-ex (ttt:apply-rule scary-a lst3))

  (run-ex (ttt:apply-rule scary-a lst1 :max-n 1))
  (run-ex (ttt:apply-rule scary-a lst2 :max-n 1))
  (run-ex (ttt:apply-rule scary-a lst3 :max-n 1)))

;; Now we define an identical, but incorrectly named rule.
(defun make-scary (x) (make-scary! x))
(setq bad-all-scary-rule
     '(/ _! (make-scary _!)))
(setq bad-scary-a
     '(/ a (make-scary a)))
(defun run-bad-example-set1 ()
  (run-ex (ttt:apply-rule bad-all-scary-rule lst1 :max-n 1))
  (run-ex (ttt:apply-rule bad-all-scary-rule lst2 :max-n 1))
  (run-ex (ttt:apply-rule bad-all-scary-rule lst3 :max-n 1))

  (run-ex (ttt:apply-rule bad-scary-a lst1 :max-n 1))
  (run-ex (ttt:apply-rule bad-scary-a lst2 :max-n 1))
  (run-ex (ttt:apply-rule bad-scary-a lst3 :max-n 1)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Interesing example 'negate-vp!'
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Now onto something a bit more useful.  VP negation is a bit tricky and
;; requires a little manipulation of the segment.  For example:
;;  "I am happy" -> "I am not happy"
;;  (I.pro ((pres be.v) happy.a)) 
;;    -> (I.pro ((pres be.v) not.adv-a happy.a))
;;
;;  "I went home" -> "I did not go home"
;;  (I.pro ((past go.v) home.v)) 
;;    -> (I.pro ((past do.aux-s) not.adv-s (go.v (k home.n))))
;;
;; The rule negate-vp! is a bit complicated, but can be found in 
;; ttt-preds-and-functions.lisp.  Basically, what it does is split up the
;; verb phrase into the head verb/auxiliary, tense and the remaining components,
;; then inserts the negation after the head verb -- adding 'do.aux-s' as
;; appropriate.

(setq ulf1 '(I.pro ((pres be.v) happy.a)))
(setq ulf2 '(I.pro ((pres go.v) (k home.n))))

;; Negates a simple sentence.
(setq negate-rule
      '(/ (_! ((! (tense? verbaux?)) _!2))
          (_! (negate-vp! (! _!2)))))

(defun run-example-set2 ()
  (run-ex (ttt:apply-rule negate-rule ulf1 :max-n 1))
  (run-ex (ttt:apply-rule negate-rule ulf2 :max-n 1)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Interesing example 'non-cf-version!'
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This next mapping function takes a counterfactual verb phrase and returns
;; the non-counterfactual version, e.g.
;;  "(If I) were rich" -> "am rich"
;;  ((cf were.v) rich.a) -> ((pres be.v) rich.a)
;;
;;  "(If I) had been rich" -> "was rich"
;;  ((cf perf) (be.v rich.a)) -> ((past be.v) rich.a

(setq ulf3 '(I ((pres wish.v) (tht (I.pro ((cf have.v) (a.d car.n)))))))
(setq ulf4 '(I ((pres wish.v) (tht (I.pro ((cf perf) (have.v (a.d car.n))))))))

;; Maps to non-counterfactual.
(setq non-cf-rule
      '(/ (_! ((cf (! verbaux?)) _!2))
          (_! (non-cf-version! ((cf !) _!2)))))

(defun run-example-set3 ()
  (run-ex (ttt:apply-rule non-cf-rule ulf3 :max-n 1))
  (run-ex (ttt:apply-rule non-cf-rule ulf4 :max-n 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Combining 'negate-vp!' and 'non-cf-version!'
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Notice counterfactual inferences are  the negative forms of the
;; non-counterfactual versions,
;;  "If I were rich" => "I am not rich"
;;  "If I had been rich" => "I was not rich"

;; Here is a simple counterfactual inference rule.  If you look at 
;; *infer-falsehood-from-positive-counterfactual* in 
;; counterfactual-inferences.lisp, you'll see that the rule there at the core
;; is the same as this with some extra flexibility of allowing adverbials and
;; restrictions on the subject.
(setq cf-inf-rule
      '(/ (_! ((cf (! verbaux?)) _!2))
          (_! (negate-vp! (non-cf-version! ((cf !) _!2))))))

;; This example uses 'all-rule-result' which is a function defined in 
;; util-from-pilot-project.lisp.  It runs the rule shallowly on all subtrees
;; and returns non-null results.  This allows the inference to get lifted out
;; of it context.
(defun run-example-set4 ()
  (run-ex (all-rule-result cf-inf-rule ulf3))
  (run-ex (all-rule-result cf-inf-rule ulf4)))

