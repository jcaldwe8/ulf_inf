; Dual inferences from requests
; `````````````````````````````
(setq *req* '((pres would.aux-v) you.pro please.adv-s speak_up.v))
(format t "~a" (ttt:apply-rule *infer-want-and-expect-from-request* *req*))
; ((I.PRO WANT.V (THAT (YOU.PRO SPEAK_UP.V)))
;  (I.PRO EXPECT.V (THAT (YOU.PRO SPEAK_UP.V))))

(setq *req2* '(((pres can.aux-v) somebody.pro (help.v me.pro)) [?]))
(format t "~a" (ttt:apply-rule *infer-want-and-expect-from-request* *req2*))
; ((I.PRO WANT.V (THAT (SOMEBODY.PRO (HELP.V ME.PRO))))
;  (I.PRO EXPECT.V (THAT (SOMEBODY.PRO (HELP.V ME.PRO)))))

(setq *req3* '((Please.adv-s 
                ((pres will.aux-s) you.pro (close.v (the.d door.n)
                 (adv-e (when.ps (you.pro ((pres go.v) out.adv-a)))))))))
(format t "~a" (ttt:apply-rule *infer-want-and-expect-from-request* *req3*))
; ((I.PRO WANT.V
;   (THAT
;    (YOU.PRO
;     (CLOSE.V (THE.D DOOR.N)
;      (ADV-E (WHEN.PS (YOU.PRO ((PRES GO.V) OUT.ADV-A))))))))
;  (I.PRO EXPECT.V
;   (THAT
;    (YOU.PRO
;     (CLOSE.V (THE.D DOOR.N)
;      (ADV-E (WHEN.PS (YOU.PRO ((PRES GO.V) OUT.ADV-A)))))))))

(setq *req4* '((((pres could.aux-v) you.pro (turn_on.v (the.d light.n)))
                 please.adv-s) [?]))
(format t "~a" (ttt:apply-rule *infer-want-and-expect-from-request* *req4*))
; ((I.PRO WANT.V (THAT (YOU.PRO (TURN_ON.V (THE.D LIGHT.N)))))
;  (I.PRO EXPECT.V (THAT (YOU.PRO (TURN_ON.V (THE.D LIGHT.N))))))

; Inferences from counterfactual wishing
;```````````````````````````````````````
(setq *wish* '(i.pro ((pres wish.v)
                      (tht (he.pro (would-cf.aux-s (finish.v it.pro)))))))
(format t "~a"
  (ttt:apply-rule *infer-falsehood-from-positive-counterfactual-wish* *wish*))
; (HE.PRO ((PRES WILL.AUX-S) NOT.ADV-S (FINISH.V IT.PRO)))

(setq *wish2* '(i.pro ((pres wish.v) (tht (he.pro (would-cf.aux-v leave.v))))))
(format t "~a"
  (ttt:apply-rule *infer-falsehood-from-positive-counterfactual-wish* *wish2*))
; (HE.PRO ((PRES WILL.AUX-V) NOT.ADV-S LEAVE.V))

(setq *wish3*
'((I.pro ((pres wish.v) 
          (tht (she.pro (would-cf.aux-s 
                           (stop.v 
                             (ka (play.v (that.d (stupid.a music.n)))))))))) \.))
(format t "~a"
  (ttt:apply-rule *infer-falsehood-from-positive-counterfactual-wish* *wish3*))
; (SHE.PRO
;  ((PRES WILL.AUX-S) NOT.ADV-S
;   (STOP.V (KA (PLAY.V (THAT.D (STUPID.A MUSIC.N)))))))

; Inferences from positive counterfactual conditionals
; ````````````````````````````````````````````````````
(setq *if* 
  '(if.md (I.pro (were-cf.v rich.a))
          (i.pro ((pres would.aux-v) (travel.v (to-arg.p |Rome|))))))
(format t "~a"
 (ttt:apply-rule *infer-falsehood-from-positive-counterfactual-if* *if*))
; (I.PRO ((PRES BE.V) NOT.ADV-S RICH.A))

(setq *if2*
  '(if.cc (she.pro (had-cf.v (a.d hammer.n))) 
          (she.pro ((pres would.aux-v (swing.v it.pro))))))
(format t "~a"
 (ttt:apply-rule *infer-falsehood-from-positive-counterfactual-if* *if2*))
; (SHE.PRO ((PRES DO.AUX-S) NOT.ADV-S (HAVE.V (A.D HAMMER.N))))

(setq *if3*
'(if.cc (she.pro (perf-cf (have.v (a.d hammer.n))))
        (she.pro ((past would.aux-v) ((perf swing.v) it.pro)))))
(format t "~a"
 (ttt:apply-rule *infer-falsehood-from-positive-counterfactual-if* *if3*))
; (SHE.PRO ((PAST DO.AUX-S) NOT.ADV-S (HAVE.V (A.D HAMMER.N))))

; 

; Inferences from relations between attitudes
; ```````````````````````````````````````````
