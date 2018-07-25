;;; Gene Kim 7-24-2018
;;; Ported from formulas defined by Len to test the pilot inference system.

;; Define applicable rule subset for request and counterfactuals.
(defparameter *rule-names* 
 '(infer-want-from-request infer-expect-from-request 
   infer-falsehood-from-positive-counterfactual 
   infer-falsehood-from-inverted-positive-counterfactual
   infer-fact-from-negative-counterfactual
   infer-fact-from-inverted-negative-counterfactual
 ))
;; Run the select rule subset on the given ULF and return a list of:
;; (ulf result1 result2 ...)
(defun run-subset-rules (ulf)
  (car (results-from-applying-rules *rule-names* (list ulf) t)))


(defun list-assert-equal (result expect)
  (assert-equal (length result) (length expect) 
                (length result) (length expect)
                result expect)
  (mapcar #'(lambda (x) (assert-equal (first x) (second x)
                                      (first x) (second x)))
          (mapcar #'list result expect)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define test formulas.
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-test test-request1
  "Subset rule test on the sentence \"Would you please speak up\"" 
  (:tag :rq :subset-rules :len-pilot)
  (let* ((ulf 
           '((pres would.aux-v) you.pro please.adv-s speak_up.v))
         (expected
           '((i.pro ((pres want.v) (that (you.pro speak_up.v))))
             (i.pro ((pres expect.v) (that (you.pro speak_up.v))))))
         (result (run-subset-rules ulf)))
    (assert-equal (first result) ulf 
                  (first result) ulf)
    (list-assert-equal (cdr result) expected)))

;; "Can somebody help me?"
(setq *req2* '(((pres can.aux-v) somebody.pro (help.v me.pro)) ?))
;; "Please will you close the door when you go out"
(setq *req3* '((Please.adv-s
                ((pres will.aux-s) you.pro (close.v (the.d door.n)
                 (adv-e (when.ps (you.pro ((pres go.v) out.adv-a)))))))))
;; "Could you turn on the light please"
(setq *req4* '((((pres could.aux-v) you.pro (turn_on.v (the.d light.n)))
                 please.adv-s) ?))
;; "I wish he would finish it"
(setq *wish* '(i.pro ((pres wish.v)
                      (tht (he.pro (would-cf.aux-s (finish.v it.pro)))))))
;; "I wish he would leave"
(setq *wish2* '(i.pro ((pres wish.v) (tht (he.pro (would-cf.aux-v leave.v))))))
;; "I wish she would stop playing that stupid music."
(setq *wish3*
  '((I.pro ((pres wish.v)
          (tht (she.pro (would-cf.aux-s
                           (stop.v
                             (ka (play.v (that.d (stupid.a music.n)))))))))) \.))
;; "If I were rich I would travel to Rome"
(setq *if*
  '(if.c (I.pro (were-cf.v rich.a))
         (i.pro ((pres would.aux-v) (travel.v (to-arg.p |Rome|))))))
;; "If she had a hammer she would swing it"
(setq *if2*
  '(if.c (she.pro (had-cf.v (a.d hammer.n)))
         (she.pro ((pres would.aux-v (swing.v it.pro))))))
;; "If she had had a hammer she would swing it"
(setq *if3*
  '(if.c (she.pro (perf-cf (have.v (a.d hammer.n))))
         (she.pro ((past would.aux-v) ((perf swing.v) it.pro)))))
;; "If I were you I would be able to succeed."
(setq if-17577
  '(((adv-s (If.ps (I.pro (were-cf.v you.pro))))
    (I.pro ((pres would.aux-s) (be.v (able.v (to succeed.a)))))) \.))
;; "If you were to fall from that bridge, 
;;  it would be almost impossible to rescue you."
(setq if-17955
  '(((adv-s (If.ps (you.pro (were-cf.v (to (fall.v (adv-a (from.p 
      (that.d bridge.n))))))))) \, (it.pro ((pres would.aux-s) (be.v 
        (almost.adv-a (impossible.a (adv-a (to.p (rescue.v you.pro))))))))) \.))
;; "If I had money, I would pay what I owe you."
(setq if-18636
  '(((adv-s (If.ps (I.pro (had-cf.v (k money.n))))) \,
    (I.pro ((pres would.aux-s) (pay.v (sub what.pro (I.pro 
                              ((pres owe.v) you.pro *h))))))) \.))
;; "If only I had taken your advice."
(setq if-16988
  '(((adv-s (If.ps (only.adv-s (I.pro (perf-cf (take.v (your.d advice.n))))))) {ref1}.s) \.))
;; "Had I known your telephone number, I would have called you"
(setq inv-16968
  '(((adv-s (perf-cf I.pro (know.v (your.d (telephone.n number.n))))) \,
    (I.pro ((past would.aux-s) (perf (call.v you.pro))))) \.))
;; "I had known your telephone number, I would have called you."
(setq inv-16968-v2
  '(({if}.c (I.pro (perf-cf (know.v (your.d (telephone.n number.n)))))
            (I.pro ((past would.aux-s) (perf (call.v you.pro))))) \.))
;; "Were I rich, I would help the poor."
(setq inv-18529
  '(((adv-s (Were-cf.v I.pro rich.a)) \,
    (I.pro ((pres would.aux-s) (help.v (the.d (poor.a {ref1}.n)))))) \.))
;; "I were rich I would help the poor."
(setq inv-18529-v2
  '(({if}.c (I.pro (Were-cf.v rich.a)) 
           (I.pro ((pres would.aux-s) (help.v (the.d (poor.a {ref1}.n)))))) \.))
;; "Could you dial for me?"
(setq req-1661
  '(((pres Could.aux-v) you.pro ((dial.v {ref1}.pro) (adv-a (for.p me.pro)))) ?))
;; "Could you please repeat that?"
(setq req-2242
  '(((pres Could.aux-v) you.pro please.adv-s (repeat.v that.pro)) ?))
;; "I wish I could go to Japan."
(setq wish-1393
  '((I.pro ((pres wish.v)
          (tht (I.pro ((pres could.aux-v)
                        (go.v (adv-a (to.p |Japan|)))))))) \.))
;; "I wish I could go to Japan."
(setq wish-1393-v2
  '((I.pro ((pres wish.v)
           (tht (I.pro (could-cf.aux-v 
                        (go.v (adv-a (to.p |Japan|)))))))) \.))
;; "I wish she would stop playing that stupid music."
(setq wish-2470
  '((I.pro ((pres wish.v)
          (tht (she.pro ((pres would.aux-s)
                          (stop.v (ka (play.v (that.d (stupid.a music.n)))))))))) \.))
;; "I wish she would stop playing that stupid music."
(setq wish-2470-v2
  '((I.pro ((pres wish.v)
          (tht (she.pro (would-cf.aux-s
                          (stop.v (ka (play.v (that.d (stupid.a music.n)))))))))) \.))

;(defparameter *test-ulfs*
;  (list *req* *req2* *req3* *req4* *wish* *wish2* *wish3* *if* *if2* *if3* 
;   if-17577 if-17955 if-18636 if-16988 inv-16968 inv-16968-v2 inv-18529 
;   inv-18529-v2 req-1661 req-2242 wish-1393 wish-1393-v2 wish-2470 wish-2470-v2))
