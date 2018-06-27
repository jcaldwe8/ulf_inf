;-------------------------------------------------------------
; Lexical semantic axioms for (mostly) attitude verbs
;   -Karl Stratos (7/11) 
;-------------------------------------------------------------
(in-package epi)

; for queries that take over 10 seconds, use my-pq and my-dq 
(defun my-pq (q1)
  (pq (p q1) :stop-fn-on-answers #'has-been-answered?))
(defun my-dq (q1)
  (dq (p q1) :stop-fn-on-answers #'has-been-answered?))

; declaration: store unknown predicates in *lexicon-kb*. 
; ^^^^^^^^^^^
(store (x-is-pred-mod 'at-least-occasionally.adv) *lexicon-kb*)
(store (x-is-pred-mod 'can.v) *lexicon-kb*)
(store (x-is-predicate 'advocate.v) *lexicon-kb*)
(store (x-is-predicate 'allege.v) *lexicon-kb*)
(store (x-is-predicate 'appear.v) *lexicon-kb*)
(store (x-is-predicate 'argue.v) *lexicon-kb*)
(store (x-is-predicate 'attack.v) *lexicon-kb*)
(store (x-is-predicate 'attempt.v) *lexicon-kb*)
(store (x-is-predicate 'blame.v) *lexicon-kb*)
(store (x-is-predicate 'claim.v) *lexicon-kb*)
(store (x-is-predicate 'criticize.v) *lexicon-kb*)
(store (x-is-predicate 'doubt.v) *lexicon-kb*)
(store (x-is-predicate 'enable.v) *lexicon-kb*)
(store (x-is-predicate 'enjoy.v) *lexicon-kb*)
(store (x-is-predicate 'guess.v) *lexicon-kb*)
(store (x-is-predicate 'hold.v) *lexicon-kb*)
(store (x-is-predicate 'intend.v) *lexicon-kb*)
(store (x-is-predicate 'like.v) *lexicon-kb*)
(store (x-is-predicate 'predict.v) *lexicon-kb*)
(store (x-is-predicate 'seem.v) *lexicon-kb*)
(store (x-is-predicate 'speculate.v) *lexicon-kb*)
(store (x-is-predicate 'strive.v) *lexicon-kb*)
(store (x-is-predicate 'struggle.v) *lexicon-kb*)
(store (x-is-predicate 'surmise.v) *lexicon-kb*)
(store (x-is-predicate 'suppose.v) *lexicon-kb*)
(store (x-is-predicate 'suspect.v) *lexicon-kb*)
(store (x-is-predicate 'try.v) *lexicon-kb*)

; If X thinks that W, X believes that probably W. 
(s '(all_wff w 
     (all x ((x think.v (that w)) =>
	     (x believe.v (that (probably w)))))))

; If X predicts that W, probably X believes that W. 
(s '(all_wff w 
     (all x ((x predict.v (that w)) =>
	     (probably (x believe.v (that w)))))))

; If X predicts that W, X believes that probably W. 
(s '(all_wff w 
     (all x ((x predict.v (that w)) =>
	     (x believe.v (that (probably w)))))))

; If X says that W, probably X believes that W. 
(s '(all_wff w 
     (all x ((x say.v (that w)) =>
	     (probably (x believe.v (that w)))))))

; If X says that W, X believes that probably W. 
(s '(all_wff w 
     (all x ((x say.v (that w)) =>
	     (x believe.v (that (probably w)))))))

; If X supposes that W, X believes that probably W. 
(s '(all_wff w 
     (all x ((x suppose.v (that w)) =>
	     (x believe.v (that (probably w)))))))

; If X guesses that W, X believes that probably W. 
(s '(all_wff w 
     (all x ((x guess.v (that w)) =>
	     (x believe.v (that (probably w)))))))

; If X speculates that W, X believes that probably W. 
(s '(all_wff w 
     (all x ((x speculate.v (that w)) =>
	     (x believe.v (that (probably w)))))))

; If X suspects that W, X believes that probably W. 
(s '(all_wff w 
     (all x ((x suspect.v (that w)) =>
	     (x believe.v (that (probably w)))))))

; If X surmises that W, X believes that probably W. 
(s '(all_wff w 
     (all x ((x surmise.v (that w)) =>
	     (x believe.v (that (probably w)))))))

; If X advocates that W, probably X believes that W. 
(s '(all_wff w 
     (all x ((x advocate.v (that w)) =>
	     (probably (x believe.v (that w)))))))

; If X argues that W, probably X believes that W. 
(s '(all_wff w 
     (all x ((x argue.v (that w)) =>
	     (probably (x believe.v (that w)))))))

; If X claims that W, probably X believes that W. 
(s '(all_wff w 
     (all x ((x claim.v (that w)) =>
	     (probably (x believe.v (that w)))))))

; If X holds that W, X believes that W. 
(s '(all_wff w 
     (all x ((x hold.v (that w)) =>
	     (x believe.v (that w))))))

; If X appears/seems to P, X probably P. 
(s '(all_pred p 
     (all x ((x appear.v (ka p)) => 
	     (probably (x p))))))

(s '(all_pred p 
     (all x ((x seem.v (ka p)) => 
	     (probably (x p))))))     

; If it appears/seems that W, probably W.
(s '(all_wff w ((it appear.v (that w)) => (probably w))))

(s '(all_wff w ((it seem.v (that w)) => (probably w))))	     

; If X doubt that W, probably X believe that W is false. 
(s '(all_wff w 
     (all x ((x doubt.v (that w)) =>
	     (not (x believe.v (that w)))))))

; If X doubt that W, X believe that W is probably false. 
(s '(all_wff w 
     (all x ((x doubt.v (that w)) =>
	     (x believe.v (that (probably (not w))))))))

; If X attempts to P, X wants to P. 
(s '(all_pred p 
     (all x ((x attempt.v (ka p)) => 
	     (x want.v (ka p))))))

; If X intends to P, X wants to P. 
(s '(all_pred p 
     (all x ((x intend.v (ka p)) => 
	     (x want.v (ka p))))))

; If X tries to P, X wants to P. 
(s '(all_pred p 
     (all x ((x try.v (ka p)) => 
	     (x want.v (ka p))))))

; If X strives to P, X wants to P. 
(s '(all_pred p 
     (all x ((x strive.v (ka p)) => 
	     (x want.v (ka p))))))

; If X struggles to P, X wants to P. 
(s '(all_pred p 
     (all x ((x struggle.v (ka p)) => 
	     (x want.v (ka p))))))

; If X attacks Y for doing P, then X believes Y P. 
(s '(all_pred p
     (all x (all y ((x attack.v y (for (ka p))) =>
		    (x believe.v (that (y p))))))))

; If X blames Y for doing P, then X believes Y P. 
(s '(all_pred p
     (all x (all y ((x blame.v y (for (ka p))) =>
		    (x believe.v (that (y p))))))))

; If X criticizes Y for doing P, then X believes Y P. 
(s '(all_pred p
     (all x (all y ((x criticize.v y (for (ka p))) =>
		    (x believe.v (that (y p))))))))

; If X enjoys doing P, X at least occasionally P.
(s '(all_pred p 
     (all x ((x enjoy.v (ka p)) => (x (at-least-occasionally.adv p))))))

; If X likes doing P, X at least occasionally P.
(s '(all_pred p 
     (all x ((x like.v (ka p)) => (x (at-least-occasionally.adv p))))))

; If X loves doing P, X at least occasionally P.
(s '(all_pred p 
     (all x ((x love.v (ka p)) => (x (at-least-occasionally.adv p))))))

; If X enable Y to P, Y can P. 
(s '(all_pred p
     (all x (all y ((x enable.v (ka p) y) => (y (can.v p)))))))


