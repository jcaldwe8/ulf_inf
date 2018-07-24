; For setting up a list of test-ulfs, as parameter *test-ulfs*


(setq *req* '((pres would.aux-v) you.pro please.adv-s speak_up.v))
(setq *req2* '(((pres can.aux-v) somebody.pro (help.v me.pro)) ?))
(setq *req3* '((Please.adv-s
                ((pres will.aux-s) you.pro (close.v (the.d door.n)
                 (adv-e (when.ps (you.pro ((pres go.v) out.adv-a)))))))))
(setq *req4* '((((pres could.aux-v) you.pro (turn_on.v (the.d light.n)))
                 please.adv-s) ?))
(setq *wish* '(i.pro ((pres wish.v)
                      (tht (he.pro (would-cf.aux-s (finish.v it.pro)))))))
(setq *wish2* '(i.pro ((pres wish.v) (tht (he.pro (would-cf.aux-v leave.v))))))
(setq *wish3*
  '((I.pro ((pres wish.v)
          (tht (she.pro (would-cf.aux-s
                           (stop.v
                             (ka (play.v (that.d (stupid.a music.n)))))))))) \.))
(setq *if*
  '(if.c (I.pro (were-cf.v rich.a))
         (i.pro ((pres would.aux-v) (travel.v (to-arg.p |Rome|))))))
(setq *if2*
  '(if.c (she.pro (had-cf.v (a.d hammer.n)))
         (she.pro ((pres would.aux-v (swing.v it.pro))))))
(setq *if3*
  '(if.c (she.pro (perf-cf (have.v (a.d hammer.n))))
         (she.pro ((past would.aux-v) ((perf swing.v) it.pro)))))
(setq if-17577
  '(((adv-s (If.ps (I.pro (were-cf.v you.pro))))
    (I.pro ((pres would.aux-s) (be.v (able.v (to succeed.a)))))) \.))
(setq if-17955
  '(((adv-s (If.ps (you.pro (were-cf.v (to (fall.v (adv-a (from.p 
      (that.d bridge.n))))))))) \, (it.pro ((pres would.aux-s) (be.v 
        (almost.adv-a (impossible.a (adv-a (to.p (rescue.v you.pro))))))))) \.))
(setq if-18636
  '(((adv-s (If.ps (I.pro (had-cf.v (k money.n))))) \,
    (I.pro ((pres would.aux-s) (pay.v (sub what.pro (I.pro 
                              ((pres owe.v) you.pro *h))))))) \.))
(setq if-16988
  '(((adv-s (If.ps (only.adv-s (I.pro (perf-cf (take.v (your.d advice.n))))))) {ref1}.s) \.))
(setq inv-16968
  '(((adv-s (perf-cf I.pro (know.v (your.d (telephone.n number.n))))) \,
    (I.pro ((past would.aux-s) (perf (call.v you.pro))))) \.))
(setq inv-16968-v2
  '(({if}.c (I.pro (perf-cf (know.v (your.d (telephone.n number.n)))))
            (I.pro ((past would.aux-s) (perf (call.v you.pro))))) \.))
(setq inv-18529
  '(((adv-s (Were-cf.v I.pro rich.a)) \,
    (I.pro ((pres would.aux-s) (help.v (the.d (poor.a {ref1}.n)))))) \.))
(setq inv-18529-v2
  '(({if}.c (I.pro (Were-cf.v rich.a)) 
           (I.pro ((pres would.aux-s) (help.v (the.d (poor.a {ref1}.n)))))) \.))
(setq req-1661
  '(((pres Could.aux-v) you.pro ((dial.v {ref1}.pro) (adv-a (for.p me.pro)))) ?))
(setq req-2242
  '(((pres Could.aux-v) you.pro please.adv-s (repeat.v that.pro)) ?))
(setq wish-1393
  '((I.pro ((pres wish.v)
          (tht (I.pro ((pres could.aux-v)
                        (go.v (adv-a (to.p |Japan|)))))))) \.))
(setq wish-1393-v2
  '((I.pro ((pres wish.v)
           (tht (I.pro (could-cf.aux-v 
                        (go.v (adv-a (to.p |Japan|)))))))) \.))
(setq wish-2470
  '((I.pro ((pres wish.v)
          (tht (she.pro ((pres would.aux-s)
                          (stop.v (ka (play.v (that.d (stupid.a music.n)))))))))) \.))
(setq wish-2470-v2
  '((I.pro ((pres wish.v)
          (tht (she.pro (would-cf.aux-s
                          (stop.v (ka (play.v (that.d (stupid.a music.n)))))))))) \.))

(defparameter *test-ulfs*
  (list *req* *req2* *req3* *req4* *wish* *wish2* *wish3* *if* *if2* *if3* 
   if-17577 if-17955 if-18636 if-16988 inv-16968 inv-16968-v2 inv-18529 
   inv-18529-v2 req-1661 req-2242 wish-1393 wish-1393-v2 wish-2470 wish-2470-v2))
