;; Gene Kim 8-15-2018
;; Test cases based on formulas from the pilot inference system's evaluation.
;; This file contains just the sentences that were sampled selectively.

;; Macro for running a test to reduce the size of the test declarations.
;; Arguments
;;  name:     The name of the test (as a symbol, i.e. unquoted)
;;  sentence: String surface sentence being tested
;;  ulf:      Source ULF
;;  expected: List of expected output ULFs (in order TODO: make the order not
;;            matter)
(defmacro define-pilot-select-sampled-test (name sentence tags ulf expected)
  `(define-test ,name
     ,(format nil "Pilot experiment select sampled sentence '~a'" sentence) 
     (:tag :infer-all :select-pilot ,@tags)
     (let ((actual (mapcar #'result-formula (infer-all ,ulf)))
           (expected ,expected))
        (list-assert-equal actual expected))))

;; 
;; If statements
;;

(define-pilot-select-sampled-test
  pilot-select-sampled-1
  (:if-then)
  "If anyone was to ask what the point of the story is, I really don't know."
  '((If.ps (anyone.pro 
            ((cf be.v) (to (ask.v  
                              (ans-to (sub what.pro 
                                           ((the.d (point-of.n (the.d story.n))) 
                                            ((pres be.v) (= *h)))))))))) 
    (I.pro really.adv-s 
          ((pres do.aux-s) not.adv-s 
                           (know.v {(ans-to (sub what.pro 
                                                 ((the.d (point-of.n (the.d story.n)))
                                                  ((pres be.v) (= *h)))))}))))
  '((nobody.pro 
      ((pres will.aux-s)
       (ask.v (ans-to (sub what.pro ((the.d (point-of.n (the.d story.n)))
                                     ((pres be.v) (= *h))))))))))
              
(define-pilot-select-sampled-test
  pilot-select-sampled-2
  (:if-then)
  "If you lend someone $20 and then never see that person again, it was probably worth it."
  '((If.ps (you.pro (((pres lend.v) someone.pro (ds currency "$20")) 
                     and_then.cc
                     (never.adv-s (((pres see.v) (that.d person.n)) again.adv-a)))))
   (it.pro ((past be.v) probably.adv-s worth_it.a)))
  '())

(define-pilot-select-sampled-test
  pilot-select-sampled-3
  (:if-then)
  "If we knew what we were doing, it wouldn't be called research."
  '((If.ps 
      (we.pro ((cf know.v) 
               (ans-to (sub what.pro (we.pro ((cf prog) (do.v *h)))))))) 
    (it.pro ((cf will.aux-s) not.adv-s ((pasv call.v) (k research.n)))))
  '((we.pro ((pres do.aux-s) not.adv-s
             (know.v (ans-to (sub what.pro 
                                  (we.pro ((pres prog) (do.v *h))))))))
    (it.pro ((pres (pasv call.v)) (k research.n)))))

(define-pilot-select-sampled-test
  pilot-select-sampled-4
  (:if-then)
  "If I were in your situation, I would do the same thing."
  '((If.ps (I.pro ((cf were.v) (in.p (your.d situation.n))))) 
     (I.pro ((cf will.aux-s) (do.v (the.d (same.a thing.n)))))) 
  '((I.pro ((pres be.v) not.adv-s (in.p (your.d situation.n))))
    (I.pro ((pres will.aux-s) not (do.v (the.d (same.a thing.n)))))))

(define-pilot-select-sampled-test
  pilot-select-sampled-5
  (:if-then)
  "If only you had told me the whole story at that time!"
  '((If.ps (only.adv-s (you.pro (((cf perf) (tell.v me.pro (the.d (whole.a story.n))))
                                         (adv-e (at.p (that.d time.n)))))))
    {ref1}.s)
  '((you.pro ((past do.aux-s) not.adv-s
              (tell.v me.pro (the.d (whole.a story.n)))
              (adv-e (at.p (that.d time.n)))))))

(define-pilot-select-sampled-test
  pilot-select-sampled-6
  (:if-then)
  "If there were no air, man could not live even ten minutes."
  '((If.ps (there.pro ((cf were.v) (no.d air.n)))) 
     ((k man.n) ((cf can.aux-v) not.adv-s
                 (live.v even.adv-a (ten.d (plur minute.n))))))
  '((there.pro ((pres be.v) (k air.n)))
    ((k man.n) ((pres can.aux-v) (live.v (ten.d (plur minute.n)))))))

(define-pilot-select-sampled-test
  pilot-select-sampled-7
  (:if-then)
  "If I were rich, I would do so."
  '((If.ps (I.pro ((cf were.v) rich.a))) 
     (I.pro ((cf will.aux-s) (do.v so.adv-a))))
  '((I.pro ((pres be.v) not.adv-s rich.a))
    (I.pro ((pres will.aux-s) not.adv-s (do.v so.adv-a)))))

(define-pilot-select-sampled-test
  pilot-select-sampled-8
  (:if-then)
  "As it is, I can do nothing."
  '((As.ps (It.pro (pres be.v))) 
    (I.pro ((pres can.aux-v) (do.v nothing.pro)))) 
  '())

(define-pilot-select-sampled-test
  pilot-select-sampled-9
  (:if-then)
  "If wishes were horses, beggars might ride."
  '((If.ps ((k (plur wish.n)) ((cf were.v) (= (k (plur horse.n)))))) 
     ((k (plur beggar.n)) ((pres might.aux-s) (ride.v {(k (plur horse.n))}))))
  '(((k (plur wish.n)) 
     ((pres be.v) not.adv-s (= (k (plur horse.n)))))
    ((k (plur beggar.n)) ((pres do.aux-s) not.adv-s 
                          (ride.v (k (plur horse.n)))))))

;; This one's pretty tricky because it's hard to tell that the consequent needs
;; the antecendent to make sense.  It seems to be that the phrase "go by x" 
;; assumes a specific traveling instance or something like that...
(define-pilot-select-sampled-test
  pilot-select-sampled-10
  (:if-then)
  "If I were to go abroad, I would go by boat."
  '((If.ps (I.pro ((cf were.v) (to (go.v abroad.adv-a))))) 
    (I.pro ((cf will.aux-s) (go.v (adv-a (by.p (k boat.n)))))))
  '((I.pro ((pres will.aux-s)
            not.adv-s
            (go.v abroad.adv-a)))))

(define-pilot-select-sampled-test
  pilot-select-sampled-11
  (:if-then)
  "If only we had a garden!"
  '((If.ps (only.adv-s (we.pro ((cf have.v) (a.d garden.n))))) {ref1}.s)
  '((we.pro ((pres do.aux-s) not.adv-s
             (have.v (a.d garden.n))))))

(define-pilot-select-sampled-test
  pilot-select-sampled-12
  (:if-then)
  "If you were to win the lottery, what would you buy with the money?"
  '(((If.ps (you.pro ((cf were.v) (to (win.v (the-gen.d lottery.n)))))) 
     (sub what.pro 
          ((cf will.aux-s) you.pro ((buy.v *h) 
                                       (adv-a (with.p (the.d money.n))))))) ?)
  '((you.pro ((pres will.aux-s) not.adv-s
              (win.v (the-gen.d lottery.n))))))

;; The consequent here seems to imply that he will not and under no circumstances
;; will change his mind.  What causes this reading?  Is it just the exaggerated
;; antecendent condition?  Anything with an antecedent condition that is abslutely
;; impossible seems to indicate that the consequent is true.
(define-pilot-select-sampled-test
  pilot-select-sampled-13
  (:if-then)
  "If the sun were to rise in the west, I would not change my mind"
  '((If.ps ((the.d sun.n) ((cf were.v) (to (rise.v 
                                            (adv-a (in.p (the.d west.n)))))))) 
   ;; TODO: hmm what is 'would' here?  doens't seem counterfactual...  
   (I.pro ((cf will.aux-v) not.adv-s (change.v (my.d mind.n))))) 
  '(((the.d sun.n) ((pres will.aux-s) not.adv-s
                    (rise.v (adv-a (in.p (the.d west.n))))))))

(define-pilot-select-sampled-test
  pilot-select-sampled-14
  (:if-then)
  "If I were to be born again, I would be a musician"
  '((if.ps (I.pro ((cf were.v) (to ((pasv bear.v) again.adv-s)))))
    (I.pro ((cf will.aux-s) (be.v (= (a.d musician.n))))))
  '(
    (I.pro ((pres be.v) not.adv-s
            (= (a.d musician.n))))
    (I.pro ((pres will.aux-s) not
            ((pasv bear.v) again.adv-s)))))

(define-pilot-select-sampled-test
  pilot-select-sampled-15
  (:if-then)
  "If I were to tell you the truth, you would be surprised."
  '((if.ps (I.pro ((cf were.v) (to (tell.v you.pro (the.d truth.n)))))) 
     (you.pro ((cf will.aux-s) (be.v surprised.a))))
  '((I.pro ((pres will.aux-s) not.adv-s
            (tell.v you.pro (the.d truth.n))))
    (you.pro ((pres be.v) not.adv-s surprised.a))))

;;
;; Inverted if statements.
;;

(define-pilot-select-sampled-test
  pilot-select-sampled-16
  (:if-inv)
  "Had he known what was about to happen, he would have changed his plan."
  '(((cf perf) he.pro 
              (know.v (the.d (what.rel 
                              ((past be.v) 
                                (about_to.adv-s happen.v)))))) 
    (he.pro ((cf will.aux-s) (perf (change.v (his.d plan.n)))))) 
  '(
    (he.pro ((past do.aux-s)
             not.adv-s
             (change.v (his.d plan.n))))
    (he.pro ((past do.aux-s)
             not.adv-s
             (know.v (the.d (what.rel ((past be.v) (about_to.adv-s happen.v)))))))))

;; v2. -- using ans-to rather than relativizer.
(define-pilot-select-sampled-test
  pilot-select-sampled-17
  (:if-inv)
  "Had he known what was about to happen, he would have changed his plan."
  '(((cf perf) he.pro 
             (know.v
                (ans-to (what.pro ((past be.v) (about_to.adv-s happen.v)))))) 
    (he.pro ((cf will.aux-s) (perf (change.v (his.d plan.n)))))) 
  '(
    (he.pro ((past do.aux-s)
             not.adv-s
             (change.v (his.d plan.n))))
    (he.pro ((past do.aux-s) 
             not.adv-s
             (know.v (ans-to (what.pro ((past be.v) (about_to.adv-s happen.v)))))))))

(define-pilot-select-sampled-test
  pilot-select-sampled-18
  (:if-inv)
  "Had they known what was about to happen, they would have changed their plans."
  '(((cf perf) they.pro 
               (know.v (the.d (what.rel 
                                ((past be.v) 
                                 (about_to.adv-s happen.v)))))) 
    (they.pro ((cf will.aux-s) (perf (change.v (their.d (plur plan.n))))))) 
  '(
    (they.pro ((past do.aux-s)
             not.adv-s
             (change.v (their.d plan.n))))
    (they.pro ((past do.aux-s)
             not.adv-s
             (know.v (the.d (what.rel ((past be.v) (about_to.adv-s happen.v)))))))))

;; v2. -- using ans-to rather than relativizer.
(define-pilot-select-sampled-test
  pilot-select-sampled-19
  (:if-inv)
  "Had they known what was about to happen, they would have changed their plans."
  '(((cf perf) they.pro 
               (know.v
                 (ans-to (what.pro ((past be.v) (about_to.adv-s happen.v)))))) 
    (they.pro ((cf will.aux-s) (perf (change.v (their.d (plur plan.n))))))) 
  '(
    (they.pro ((past do.aux-s)
             not.adv-s
             (change.v (their.d plan.n))))
    (they.pro ((past do.aux-s) 
             not.adv-s
             (know.v (ans-to (what.pro ((past be.v) (about-to.adv-s happen.v)))))))))

(define-pilot-select-sampled-test
  pilot-select-sampled-20
  (:if-inv)
  "Had he taken his doctor's advice, he might not have died."
  '(((cf perf) he.pro (take.v (((his.d doctor.n) 's) advice.n))) 
    (he.pro ((cf might.aux-s) not.adv-s (perf die.v)))) 
  '((he.pro ((past do.aux-s)
             not.adv-s
             (take.v (((his.d doctor.n) 's) advice.n))))
    (he.pro (past die.v))))

(define-pilot-select-sampled-test
  pilot-select-sampled-21
  (:if-inv)
  "Had I arrived earlier, I could have seen Kelly."
  '(((cf perf) I.pro (arrive.v earlier.adv-e))
    (I.pro ((cf can.aux-s) (perf (see.v |Kelly|)))))
  '((I.pro ((past can.aux-s) not.adv-s (see.v |Kelly|)))
    (I.pro ((past do.aux-s)
            not.adv-s
            (arrive.v earlier.adv-e)))
    (I.pro ((past do.aux-s) not.adv-s (see.v |Kelly|)))))

(define-pilot-select-sampled-test
  pilot-select-sampled-22
  (:if-inv)
  "Were I you, I would ignore it."
  '(((cf Were.v) I.pro (= you.pro)) 
    (I.pro ((cf will.aux-s) (ignore.v it.pro))))
  '(
    ;; Should we include the one below? 
    (I.pro ((pres will.aux-s) not.adv-s (ignore.v it.pro)))
    (I.pro ((pres be.v) not.adv-s (= you.pro)))))

(define-pilot-select-sampled-test
  pilot-select-sampled-23
  (:if-inv)
  "Were I you, I would follow his advice."
  '(((cf Were.v) I.pro (= you.pro)) 
    (I.pro ((cf will.aux-s) (follow.v (his.d advice.n))))) 
  '(
    ;; Should we include the one below? 
    (I.pro ((pres will.aux-s) not.adv-s (follow.v (his.d advice.n))))
    (I.pro ((pres be.v) not.adv-s (= you.pro)))))

(define-pilot-select-sampled-test
  pilot-select-sampled-24
  (:if-inv)
  "Had he known the facts, the accident might have been avoided."
  '(((cf perf) he.pro (know.v (the.d (plur fact.n)))) 
    ((the.d accident.n) ((cf might.aux-s) (perf (pasv avoid.v)))))
  '(((the.d accident.n) (not.adv-s (pasv avoid.v)))
    (I.pro ((past do.aux-s)
            not.adv-s 
            (know.v (the.d (plur fact.n)))))))

(define-pilot-select-sampled-test
  pilot-select-sampled-25
  (:if-inv)
  "Had he known the truth, he would have told me."
  '(((cf perf) he.pro (know.v (the.d truth.n))) 
    (he.pro ((cf will.aux-s) (perf (tell.v me.pro))))) 
  '((he.pro ((past do.aux-s)
             not.adv-s
             (tell.v me.pro)))
    (I.pro ((past do.aux-s) 
            not.adv-s
            (know.v (the.d truth.n))))))

(define-pilot-select-sampled-test
  pilot-select-sampled-26
  (:if-inv)
  "Were I a bird, I would fly to you."
  '(((cf Were.v) I.pro (= (a.d bird.n))) 
     (I.pro ((cf will.aux-s) (fly.v (adv-a (to.p you.pro)))))) 
  '((I.pro ((pres will.aux-s) not.adv-s (fly.v (adv-a (to.p you.pro)))))
    (I.pro ((pres be.v) not.adv-s (= (a.d bird.n))))))

(define-pilot-select-sampled-test
  pilot-select-sampled-27
  (:if-inv)
  "Were I in your position, I would oppose that plan."
  '(((cf Were.v) I.pro (in.p (your.d position.n))) 
    (I.pro ((cf will.aux-s) (oppose.v (that.d plan.n)))))
  '((I.pro ((pres will.aux-s) not.adv-s (oppose.v (that.d plan.n))))
    (I.pro ((pres be.v) not.adv-s (in.p (your.d position.n))))))

(define-pilot-select-sampled-test
  pilot-select-sampled-28
  (:if-inv)
  "Had I realized what you intended, I would not have agreed."
  '(((cf perf) 
     I.pro 
     (realize.v
       (the.d (sub what.rel 
                   (you.pro ((past intend.v) *h)))))) 
    (I.pro ((cf will.aux-s) not.adv-s (perf agree.v)))) 
  '((I.pro ((past do.aux-s) 
            not.adv-s
            (realize.v (the.d (sub what.rel 
                                   (you.pro ((past intend.v) *h)))))))
    (I.pro (past agree.v))))

;; v2. -- ans-to instead of relativizer.
(define-pilot-select-sampled-test
  pilot-select-sampled-29
  (:if-inv)
  "Had I realized what you intended, I would not have agreed."
  '(((cf perf) 
     I.pro (realize.v
             (ans-to (what.pro 
                       (you.pro ((past intend.v) *h)))))) 
    (I.pro ((cf will.aux-s) not.adv-s (perf agree.v))))
  '((I.pro ((past do.aux-s)
            not.adv-s
            (realize.v 
              (ans-to (what.pro
                        (you.pro ((past intend.v) *h)))))))
    (I.pro (past agree.v))))

(define-pilot-select-sampled-test
  pilot-select-sampled-30
  (:if-inv)
  "Had I known about it, I would have told you."
  '(((cf perf) I.pro (know.v (about.p-arg it.pro)))
    (I.pro ((cf will.aux-s) (perf (tell.v you.pro)))))
  '((I.pro ((past do.aux-s) not.adv-s (tell.v you.pro)))
    (I.pro ((past do.aux-s) not.adv-s (know.v (about.p-arg it.pro))))))

(define-pilot-select-sampled-test
  pilot-select-sampled-31
  (:if-inv)
  "Were I in your place I would do the same thing."
  '(((cf Were.v) I.pro (in.p (your.d place.n))) 
    (I.pro ((cf will.aux-s) (do.v (the.d (same.a thing.n)))))) 
  '((I.pro ((pres will.aux-s) not.adv-s (do.v (the.d (same.a thing.n)))))
    (I.pro ((pres be.v) not.adv-s (in.p (your.d place.n))))))

(define-pilot-select-sampled-test
  pilot-select-sampled-32
  (:if-inv)
  "Were I in your position, I would do it at once."
  '(((cf Were.v) I.pro (in.p (your.d position.n)))
    (I.pro (((cf will.aux-s) (do.v it.pro)) at_once.adv-s))) 
  '((I.pro ((pres be.v) not.adv-s (in.p (your.d position.n))))
    ))
    ;; Commented out because of the reference that requires the antecedent truth.
    ;(I.pro (((pres will.aux-s) not.adv-s (do.v it.pro)) at_once.adv-s))))

(define-pilot-select-sampled-test
  pilot-select-sampled-33
  (:if-inv)
  "Were I free from work, I could read these books."
  '(((cf Were.v) I.pro (free.a (adv-a (from.p (k work.n)))))
    (I.pro ((cf can.aux-v) (read.v (these.d (plur book.n)))))) 
  '((I.pro ((pres can.aux-v) not.adv-s (read.v (these.d (plur book.n)))))
    (I.pro ((pres be.v) not.adv-s (free.a (adv-a (from.p (k work.n))))))))

(define-pilot-select-sampled-test
  pilot-select-sampled-34
  (:if-inv)
  "Were you to know the fact you would be surprised."
  '(((cf Were.v) you.pro (to (know.v (the.d fact.n)))) 
    (you.pro ((cf will.aux-s) (be.v surprised.a)))) 
  '((you.pro ((pres will.aux-s) 
            not.adv-s
            (know.v (the.d fact.n))))
    (you.pro ((pres will.aux-s) not.adv-s (be.v surprised.a)))))


;;
;; Requests
;;

(define-pilot-select-sampled-test
  pilot-select-sampled-35
  (:rq)
  "Could you call again later, please?"
  '((((pres Could.aux-v) you.pro ((call.v again.adv-a) later.adv-e))  
    please.adv-s) ?)
  '((i.pro ((pres want.v) you.pro (to ((call.v again.adv-a) later.adv-e))))
    (i.pro ((pres expect.v) (that (you.pro (((pres call.v) again.adv-a) later.adv-e)))))))

(define-pilot-select-sampled-test
  pilot-select-sampled-36
  (:rq)
  "Can you imagine what our lives would be like without electricity?"
  '(((pres Can.aux-v) you.pro 
    (imagine.v (the.d (sub what.rel 
                           ((our.d (plur life.n)) 
                              ((cf will.aux-s) 
                                  ((be.v (like.a *h))
                                   (adv-e (without.p (k electricity.n)))))))))) ?)
  '())

;; v2. -- ans-to instead of relativizer.
(define-pilot-select-sampled-test
  pilot-select-sampled-37
  (:rq)
  "Can you imagine what our lives would be like without electricity?"
  '(((pres Can.aux-v) 
    you.pro 
    (imagine.v (ans-to (sub what.pro 
                            ((our.d (plur life.n)) 
                             ((cf will.aux-s) 
                              ((be.v (like.a *h))
                               (adv-e (without.p (k electricity.n)))))))))) ?)
  '())

(define-pilot-select-sampled-test
  pilot-select-sampled-38
  (:rq)
  "Can I stay at your place?"
  '(((pres Can.aux-v) I.pro
     (stay.v (adv-a (at.p (your.d place.n))))) ?)
  '((I.pro ((pres want.v) (to (stay.v (adv-a (at.p (your.d place.n)))))))))

(define-pilot-select-sampled-test
  pilot-select-sampled-39
  (:rq)
  "Could you turn on the light please?"
  '((((pres Could.aux-v)
      you.pro
      (turn_on.v (the.d light.n))) please.adv-s) ?)
  '((i.pro ((pres want.v) you.pro (to (turn_on.v (the.d light.n)))))
    (i.pro ((pres expect.v) (that (you.pro ((pres turn_on.v) (the.d light.n))))))))

(define-pilot-select-sampled-test
  pilot-select-sampled-40
  (:rq)
  "Could you do me a favour please?"
  '((((pres Could.aux-v) you.pro 
                         (do.v me.pro (a.d favour.n))) please.adv-s) ?)
  '((i.pro ((pres want.v) you.pro (to (do.v me.pro (a.d favour.n)))))))

(define-pilot-select-sampled-test
  pilot-select-sampled-41
  (:rq)
  "Can you justify the use of violence?"
  '(((pres Can.aux-v) you.pro (justify.v (the.d (use-of.n (k violence.n))))) ?)
  '())

(define-pilot-select-sampled-test
  pilot-select-sampled-42
  (:rq)
  "Can you do bookkeeping?"
  '(((pres Can.aux-v) you.pro (do.v (ka bookkeep.v))) ?)
  '())

(define-pilot-select-sampled-test
  pilot-select-sampled-43
  (:rq)
  "Can you keep a secret?"
  '(((pres Can.aux-v) you.pro (keep.v (a.d secret.n))) ?)
  '((i.pro ((pres want.v) you.pro (to (keep.v (a.d secret.n)))))))

(define-pilot-select-sampled-test
  pilot-select-sampled-44
  (:rq)
  "Can you ride a horse?"
  '(((pres Can.aux-v) you.pro (ride.v (a-gen.d horse.n))) ?)
  '())

(define-pilot-select-sampled-test
  pilot-select-sampled-45
  (:rq)
  "Can you tell wheat from barley?"
  '(((pres Can.aux-v) you.pro
     ((tell.v (k wheat.n))
       (adv-a (from.p (k barley.n))))) ?)
  '())

(define-pilot-select-sampled-test
  pilot-select-sampled-46
  (:rq)
  "Can you throw a fastball?"
  '(((pres Can.aux-v) you.pro (throw.v (a.d fastball.n))) ?)
  '())

(define-pilot-select-sampled-test
  pilot-select-sampled-47
  (:rq)
  "Can you eat raw oysters?"
  '(((pres Can.aux-v) you.pro (eat.v (k (raw.a (plur oyster.n))))) ?)
  '())

;; TODO: filter inferenes based on kinds, generics and a.d.  can.aux-v along with these types of entities in the VP indicates an ability question rather than a request.

(define-pilot-select-sampled-test
  pilot-select-sampled-48
  (:rq)
  "Can you break away from your parents?"
  '(((pres Can.aux-v) you.pro
     (break_away.v (from.p-arg (your.d (plur (parent-of.n *s)))))) ?)
  '())

;;
;; Wishes.
;;

(define-pilot-select-sampled-test
  pilot-select-sampled-49
  (:wish)
  "I wish I could care more about my grades but it seems that, at a certain point of my life, I decided they would not be so important anymore."
  '((I.pro 
    ((pres wish.v)
       (tht
         (I.pro ((cf could.aux-v) 
                 (care.v more.adv-a (about.p-arg (my.d (plur grade.n)))))))))
   but.cc
   (it.pro 
     ((pres seem.v) 
       (that 
         ((adv-e (at.p (a.d (certain.a (n+preds point.n 
                                                 (of.n (my.d life.n))))))) 
            (I.pro 
              ((past decide.v) 
                  (tht 
                    (they.pro ((past will.aux-s) not.adv-s 
                                 ((be.v (so.adv-a important.a))
                                  anymore.adv-e)))))))))))
  '((i.pro ((pres could.aux-v) not.adv-s
            (care.v more.adv-a (about.p-arg (my.d (plur grade.n))))))))

(define-pilot-select-sampled-test
  pilot-select-sampled-50
  (:wish)
  "What famous songs do you wish you had composed, and why?"
  '((sub (What.d (famous.a (plur song.n))) 
        (((pres do.aux-s) you.pro 
            (wish.v 
              (tht
                (you.pro ((cf perf) (compose.v *h))))))  
         and.cc
         (why.adv-s 
           {((pres do.aux-s) you.pro 
               (wish.v 
                 (tht
                   (you.pro ((cf perf) (compose.v *h))))))}))) ?)
  '((you.pro ((past do.aux-s) not.adv-s
              (compose.v (what.d (famous.a (plur song.n))))))))

(define-pilot-select-sampled-test
  pilot-select-sampled-51
  (:wish)
  "I wish that you had told me the truth."
  '(I.pro ((pres wish.v)
             (tht 
               (you.pro ((cf perf) (tell.v me.pro (the.d truth.n)))))))
  '((you.pro ((past do.aux-s) not.adv-s
              (tell.v me.pro (the.d truth.n))))))

(define-pilot-select-sampled-test
  pilot-select-sampled-52
  (:wish)
  "You will wish you had never seen it."
  '(You.pro
    ((pres will.aux-s)
       (wish.v
         (tht
           (you.pro ((cf perf) never.adv-e (see.v it.pro))))))) 
  '())

(define-pilot-select-sampled-test
  pilot-select-sampled-53
  (:wish)
  "I wish you had told me the truth then."
  '(I.pro
     ((pres wish.v)
       (tht
         (you.pro 
           ((cf perf) ((tell.v me.pro (the.d truth.n)) then.adv-e))))))
  '((you.pro ((past do.aux-s) not.adv-s
              ((tell.v me.pro (the.d truth.n)) then.adv-e)))))

(define-pilot-select-sampled-test
  pilot-select-sampled-54
  (:wish)
  "Your wish will come true in the near future."
  '((Your.d wish.n)
      ((pres will.aux-s) 
          (come_true.v (adv-e (in.p (the.d (near.a future.n)))))))
  '())

(define-pilot-select-sampled-test
  pilot-select-sampled-55
  (:wish)
  "I wish I could live near your house."
  '(I.pro 
      ((pres wish.v)
         (tht
           (I.pro 
             ((cf could.aux-v) (live.v (adv-e (near.p (your.d house.n)))))))))
  '((i.pro ((pres could.aux-v) not.adv-s
            (live.v (adv-e (near.p (your.d house.n))))))))

(define-pilot-select-sampled-test
  pilot-select-sampled-56
  (:wish)
  "I wish I could go to the park with you."
  '(I.pro
     ((pres wish.v)
        (tht
          (I.pro
            ((cf could.aux-v)
              (go.v (adv-a (to.p (the.d park.n)))
                    (adv-a (with.p you.pro)))))))) 
  '((i.pro ((pres could.aux-v) not.adv-s
            (go.v (adv-a (to.p (the.d park.n)))
                  (adv-a (with.p you.pro)))))))

(define-pilot-select-sampled-test
  pilot-select-sampled-57
  (:wish)
  "I wish you could come with us."
  '(I.pro
     ((pres wish.v)
        (tht
          (you.pro ((cf could.aux-v) (come.v (adv-a (with.p us.pro))))))))
  '((you.pro ((pres could.aux-v) not.adv-s
              (come.v (adv-a (with.p us.pro)))))))

(define-pilot-select-sampled-test
  pilot-select-sampled-58
  (:wish)
  "I wish you could drop in at my house on your way home."
  '(I.pro
     ((pres wish.v)
        (tht
          (you.pro
            ((cf could.aux-v)
              (drop_in.v (adv-a (at.p (my.d house.n)))
                         (adv-a (on.p (your.d (n+preds way.n
                                                       ({toward}.p (k home.n)))))))))))) 
  '((you.pro ((pres could.aux-v) not.adv-s
              (drop_in.v (adv-a (at.p (my.d house.n)))
                         (adv-a (on.p (your.d (n+preds way.n
                                                       ({toward}.p (k home.n)))))))))))

(define-pilot-select-sampled-test
  pilot-select-sampled-59
  (:wish)
  "I wish you were close to me."
  '(I.pro
     ((pres wish.v)
       (tht
         (you.pro 
           ((cf were.v) (close.a (to.p-arg me.pro)))))))
  '((you.pro ((pres be.v) not.adv-s
              (close.a (to.p-arg me.pro))))))

(define-pilot-select-sampled-test
  pilot-select-sampled-60
  (:wish)
  "I wish you had not told the story to my mother."
  '(I.pro
    ((pres wish.v)
      (tht
        (you.pro
          ((cf perf) not.adv-s (tell.v (the.d story.n) (to.p-arg (my.d (mother-of.n *s))))))))) 
  '((you.pro ((past tell.v) (the.d story.n)
                            (to.p-arg (my.d (mother-of.n *s)))))))

(define-pilot-select-sampled-test
  pilot-select-sampled-61
  (:wish)
  "I wish I were rich."
  '(I.pro
    ((pres wish.v)
      (tht
        (I.pro ((cf were.v) rich.a)))))
  '((I.pro ((pres be.v) not.adv-s rich.a))))

(define-pilot-select-sampled-test
  pilot-select-sampled-62
  (:wish)
  "I wish our classroom were air-conditioned."
  '(I.pro
    ((pres wish.v)
      (tht
        ((our.d classroom.n) ((cf were.v) air-conditioned.a)))))
  '(((our.d classroom.n) ((pres be.v) not.adv-s air-conditioned.a))))

