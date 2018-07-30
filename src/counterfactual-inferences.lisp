;;; Gene Kim 7-23-2018
;;;
;;; Initially copied over functions written by Len for the pilot inference
;;; project.  The functions have been refactored for better overall project
;;; organization.

(defparameter *infer-falsehood-from-positive-counterfactual*
;``````````````````````````````````````````````````````````
; For a top-level counterfactual, e.g., "If I were rich ..." or "I wish
; I were rich".
;
; Does not look for "if" or "wish" explicitly to allow e.g., for 
; (if (only ...) ...); It'll also cover "John thinks that if he were rich ...",
; etc., if we search through the ulf.
;
 '(/ ((!1 ~ adv-s)   (? atom? ~ not.adv-s not never.adv-f never.adv-s)
      ;subj         ```````` poss. nonneg. adv (as sibling of subj & VP
       ((cf (!2 verbaux?))
        (?1 atom? ~ not.adv-s not never.adv-f never.adv-s)
           ;``````````````` poss. nonneg. adv (sibling of head verb)
        _!1))
     (!1 (negate-vp! (non-cf-version! ((cf !2) ? ?1 _!1))))))


(defparameter *infer-falsehood-from-inverted-positive-counterfactual*
;```````````````````````````````````````````````````````````````````
; E.g., "Had I known your telephone number ..."
 '(/ ((cf (!2 verbaux?)) 
        (!3 ~ not.adv-s not never.adv-f never.adv-s)
          ;``````````````` subj (TODO: change this to match any NP)
        (? atom? ~ not.adv-s not never.adv-f never.adv-s)
          ;``````````````` poss. nonneg. adv (sibling of head verb)
         _!1)
     (!3 (negate-vp! (non-cf-version! ((cf !2) ? _!1))))))


(defparameter *infer-fact-from-negative-counterfactual*
;``````````````````````````````````````````````````````
; E.g., "You will wish you had never seen it"
;
 '(/ ((!1 ~ adv-s)   (? not.adv-s not never.adv-f never.adv-s)
      ;subj          ```````` poss. neg. adv (as sibling of subj & VP
       ((cf (! verbaux?))
        (!2 not.adv-s not never.adv-f never.adv-s)
           ;``````` neg. adv (sibling of head verb)
           ;        NB: same variable '?' as above (double-neg ~= neg)
        _!1))
     (!1 (non-cf-version! ((cf !) _!1)))))


(defparameter *infer-fact-from-inverted-negative-counterfactual*
;```````````````````````````````````````````````````````````````
; E.g. "Had I not met her, ..."
;
 '(/ ((cf (!1 verbaux?)) _!
         (!2 not.adv-s not never.adv-f never.adv-s)
           ;``````````````` neg. adv (sibling of head verb)
         _!1)
      (_! (non-cf-version! ((cf !1) _!1)))))


; Now the functionalized rules themselves (applying the TTT rules at
; multiple depths).  These functions all return lists of 'inf-result'
; instances.

(defun infer-falsehood-from-positive-counterfactual (ulf)
;```````````````````````````````````````````````````````
 (all-ttt-rule-inf-result *infer-falsehood-from-positive-counterfactual* ulf))


(defun infer-falsehood-from-inverted-positive-counterfactual (ulf)
;````````````````````````````````````````````````````````````````
 (all-ttt-rule-inf-result *infer-falsehood-from-inverted-positive-counterfactual* ulf))


(defun infer-fact-from-negative-counterfactual (ulf)
;```````````````````````````````````````````````````
 (all-ttt-rule-inf-result *infer-fact-from-negative-counterfactual* ulf))


(defun infer-fact-from-inverted-negative-counterfactual (ulf)
;```````````````````````````````````````````````````````````
 (all-ttt-rule-inf-result *infer-fact-from-inverted-negative-counterfactual* ulf))

; [raw variants]
; The *-raw variants of the rules return lists of formulas instead of
; 'inf-result instances.
; 
(defun infer-falsehood-from-positive-counterfactual-raw (ulf)
 (mapcar #'result-formula 
         (infer-falsehood-from-positive-counterfactual ulf)))
(defun infer-falsehood-from-inverted-positive-counterfactual-raw (ulf)
 (mapcar #'result-formula
         (infer-falsehood-from-inverted-positive-counterfactual ulf)))
(defun infer-fact-from-negative-counterfactual-raw (ulf)
 (mapcar #'result-formula
         (infer-fact-from-negative-counterfactual ulf)))
(defun infer-fact-from-inverted-negative-counterfactual-raw (ulf)
 (mapcar #'result-formula
         (infer-fact-from-inverted-negative-counterfactual ulf)))


;; For development, TODO: move these to actual unit tests.
(setq gotosleep '((go.v (adv-a (to.p (k sleep.n))))))
(setq notgotosleep '(not.adv-s (go.v (adv-a (to.p (k sleep.n))))))
(setq notbeaperson '(not.adv-s (be.v (= (a.d person.n)))))
(setq beaperson '((be.v (= (a.d person.n)))))

(setq ifgotosleep '((If.ps (I.pro ((cf were.v) (to (go.v (adv-a (to.p (k sleep.n)))))))) (I.pro ((cf will.aux-s) dream.v))))
(setq ifnotgotosleep '((If.ps (I.pro ((cf were.v) (to not.adv-s (go.v (adv-a (to.p (k sleep.n)))))))) (I.pro ((cf will.aux-s) (be.v asleep.a))) ))

(defparameter *preprocess-if-were-to*
;````````````````````````````````````
; If I were to go to sleep -> If I went to sleep
; If I were ever to go to sleep -> If I ever went to sleep
; If I were to not go to sleep -> If I did not go to sleep
; If I were not to go to sleep -> If I did not go to sleep
; If I were to not be a person -> If I were not a person
;
  '(/ (if.ps (_!1 ((cf (!3 were.v be.v))
                   _? ; possible negation 
                   (to _+2))))
      (if.ps (_!1 _? (remove-were-to! (_+2))))))

(defparameter *uninvert-if-were-to-inv*
;``````````````````````````````````````
; Were I to go to sleep -> If I were to go to sleep
; Were I ever to go sleep -> If I were ever to go to sleep
  '(/ ((cf (!3 were.v be.v)) _! _? (to _+2))
      (if.ps (_! _? (to _+2)))))

;; Counterfactual-specific preprocessing.
;; 1. If x were to y, ... -> If x y, ...
(defun counterfactual-preprocess (ulf)
  (ttt:apply-rule *preprocess-if-were-to*
                  (ttt:apply-rule *uninvert-if-were-to-inv* ulf)))

;; Define functions for full pipeline.
(defun premacro-counterfactual-inferences (ulf)
  (cdar (results-from-applying-rules
          (list #'infer-falsehood-from-positive-counterfactual
                #'infer-falsehood-from-inverted-positive-counterfactual
                #'infer-fact-from-negative-counterfactual
                #'infer-fact-from-inverted-negative-counterfactual)
         (list (counterfactual-preprocess ulf)) t)))


