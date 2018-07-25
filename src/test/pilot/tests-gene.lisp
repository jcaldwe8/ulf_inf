; Gene 7-24-2018
; Adjusted from Len's tests from sentences sent by Gene for the pilot
; experiments.  Len's initial comments:
; > THIS WAS AN INITIAL ATTEMPT TO PROCESS GENE'S EXAMPLES, NOT ENTIRELY
; > SUCCESSFUL.
;==========================================================================

;; TODO: Update formulas here and turn into lisp-unit tests.

(defun f (x) (format t "~s~%" x) '_____________); for full value-printout

; 16549 You will wish you had never seen it.
(defparameter *wish0*
 '((you.pro ((pres will.aux-s) 
             (wish.v (tht (you.pro (perf-cf never.adv-f (see.v it.pro))))))) \.))
(f (ttt:apply-rule *infer-fact-from-negative-counterfactual* *wish0*))
; (YOU.PRO ((PAST DO.AUX-S) (SEE.V IT.PRO)))


(defparameter *if-1*
 '(((adv-s (if.ps (i.pro ((cf were.v) (= you.pro)))))
    (I.pro ((pres would.aux-s) (be.v (able.v (to succeed.a)))))) \.))
(f (ttt:apply-rule *infer-falsehood-from-positive-counterfactual* *if-1*)) 
; (I.PRO ((PRES BE.V) NOT.ADV-S (= YOU.PRO)))

(defparameter *if-2*
 '(((adv-s (If.ps (you.pro (were-cf.v (to (fall.v 
         (adv-a (from.p (that.d bridge.n))))))))) \,
    (it.pro ((pres would.aux-s) (be.v (almost.adv-a (impossible.a 
         (adv-a (to.p (rescue.v you.pro))))))))) \.))
(f (ttt:apply-rule *infer-falsehood-from-positive-counterfactual* *if-2*))
; (YOU.PRO
;  ((PRES BE.V) NOT.ADV-S
;   (TO (FALL.V (ADV-A (FROM.P (THAT.D BRIDGE.N)))))))

(defparameter *if-3*
 '(((adv-s (If.ps (I.pro (had-cf.v (k money.n))))) \,
    (I.pro ((pres would.aux-s) (pay.v (sub what.pro (I.pro 
                                        ((pres owe.v) you.pro *h))))))) \.))
(f (ttt:apply-rule *infer-falsehood-from-positive-counterfactual* *if-3*))
; (I.PRO ((PRES DO.AUX-S) NOT.ADV-S (HAVE.V (K MONEY.N))))

(defparameter *if-4*
 '(((adv-s (If.ps (only.adv-s (I.pro (perf-cf (take.v 
                                 (your.d advice.n))))))) {ref1}.s) \.))
(f (ttt:apply-rule *infer-falsehood-from-positive-counterfactual* *if-4*))
; DOESN'T WORK, BECAUSE OF THE "ONLY"

(f (ttt:apply-rule *infer-falsehood-from-positive-counterfactual* *if-4*))
; (I.PRO ((PAST DO.AUX-S) NOT.ADV-S (TAKE.V (YOUR.D ADVICE.N))))

; 16968   Had I known your telephone number, I would have called you.
(defparameter *inv-1*
 '(((adv-s (perf-cf I.pro (know.v (your.d (telephone.n number.n))))) \,
    (I.pro ((past would.aux-s) (perf (call.v you.pro))))) \.))
(f (ttt:apply-rule *infer-falsehood-from-inverted-positive-counterfactual* *inv-1*))
; (I.PRO
;  ((PAST DO.AUX-S) NOT.ADV-S (KNOW.V (YOUR.D (TELEPHONE.N NUMBER.N)))))

; "Had I not met her, I would be in Rome."
(defparameter *inv-neg-1*
 '(((adv-s (perf-cf i.pro not.adv-s (meet.v her.pro))) 
    (I.pro ((pres would.aux-s) (be.v (in.p |Rome|)))))))
(f (ttt:apply-rule *infer-fact-from-negative-counterfactual* *inv-neg-1*))


(ttt:match-expr '((! verb-cf?) _! (!1 not.adv-s not never.adv-f never.adv-s) _!1) 
                '(PERF-CF I.PRO NOT.ADV-S (MEET.V HER.PRO)))
