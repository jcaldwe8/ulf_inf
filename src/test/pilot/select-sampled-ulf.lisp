;; Text file that simply lists all the selectively sampled formulas.
;; This is meant to be loaded into Lisp with the 'read' function.


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


"If anyone was to ask what the point of the story is, I really don't know."
((If.ps (anyone.pro 
          ((cf was.v) (to (ask.v  
                            (ans-to (sub what.pro 
                                         ((the.d (point-of.n (the.d story.n))) 
                                          ((pres be.v) (= *h)))))))))) 
 (I.pro really.adv-s 
        ((pres do.aux-s) not.adv-s 
                         (know.v {(ans-to (sub what.pro 
                                               ((the.d (point-of.n (the.d story.n))) 
                                                ((pres be.v) (= *h)))))})))) 

"If you lend someone $20 and then never see that person again, it was probably worth it."
((If.ps (you.pro (((pres lend.v) someone.pro (ds currency "$20")) and_then.cc
                         (never.adv-s (((pres see.v) (that.d person.n)) again.adv-a)))))
   (it.pro ((past be.v) probably.adv-s worth_it.a))) 

"If we knew what we were doing, it wouldn't be called research."
((If.ps 
   (we.pro ((cf know.v) 
            (ans-to (sub what.pro (we.pro ((past prog) (do.v *h)))))))) 
 (it.pro ((cf will.aux-s) not.adv-s ((pasv call.v) (k research.n))))) 

"If I were in your situation, I would do the same thing."
((If.ps (I.pro ((cf were.v) (in.p (your.d situation.n))))) 
   (I.pro ((cf will.aux-s) (do.v (the.d (same.a thing.n)))))) 

"If only you had told me the whole story at that time!"
((If.ps (only.adv-s (you.pro (((cf perf) (tell.v me.pro (the.d (whole.a story.n))))
                                       (adv-e (at.p (that.d time.n)))))))
   {ref1}.s)

"If there were no air, man could not live even ten minutes."
((If.ps (there.pro ((cf were.v) (= (no.d air.n))))) 
   ((k man.n) ((cf can.aux-v) not.adv-s (live.v even.adv-a (ten.d (plur minute.n)))))) 

"If I were rich, I would do so."
((If.ps (I.pro ((cf were.v) rich.a))) 
   (I.pro ((cf will.aux-s) (do.v so.adv-a))))

"As it is, I can do nothing."
((As.ps (It.pro (pres be.v))) 
   (I.pro ((pres can.aux-v) (do.v nothing.pro)))) 

"If wishes were horses, beggars might ride."
((If.ps ((k (plur wish.n)) ((cf were.v) (k (plur horse.n))))) 
   ((k (plur beggar.n)) ((pres might.aux-s) (ride.v {(k (plur horse.n))})))) 

"If I were to go abroad, I would go by boat."
((If.ps (I.pro ((cf were.v) (to (go.v abroad.adv-a))))) 
   (I.pro ((cf will.aux-s) (go.v (adv-a (by.p (k boat.n)))))))

"If only we had a garden!"
((If.ps (only.adv-s (we.pro ((cf have.v) (a.d garden.n))))) {ref1}.s)

"If you were to win the lottery, what would you buy with the money?"
(((If.ps (you.pro ((cf were.v) (to (win.v (the-gen.d lottery.n)))))) 
   (sub what.pro 
        ((cf will.aux-s) you.pro ((buy.v *h) 
                                       (adv-a (with.p (the.d money.n))))))) ?)

"If the sun were to rise in the west, I would not change my mind"
((If.ps ((the.d sun.n) ((cf were.v) (to (rise.v 
                                          (adv-a (in.p (the.d west.n)))))))) 
 ;; TODO: hmm what is 'would' here?  doens't seem counterfactual...  
 (I.pro ((cf will.aux-v) not.adv-s (change.v (my.d mind.n))))) 

"If I were to be born again, I would be a musician"
((if.ps (I.pro ((cf were.v) (to ((pasv bear.v) again.adv-s)))))
   (I.pro ((cf will.aux-s) (be.v musician.n)))) 

"If I were to tell you the truth, you would be surprised."
((if.ps (I.pro ((cf were.v) (to (tell.v you.pro (the.d truth.n)))))) 
    (you.pro ((cf will.aux-s) (be.v surprised.a)))) 

;;
;; Inverted if statements.
;;

"Had he known what was about to happen, he would have changed his plan."
(((cf perf) he.pro 
            (know.v (the.d (what.rel 
                             ((past be.v) 
                              (about_to.adv-s happen.v)))))) 
 (he.pro ((cf will.aux-s) (perf (change.v (his.d plan.n)))))) 
;; v2. -- using ans-to rather than relativizer.
(((cf perf) he.pro 
            (know.v
              (ans-to (what.pro ((past be.v) (about_to.adv-s happen.v)))))) 
 (he.pro ((cf will.aux-s) (perf (change.v (his.d plan.n)))))) 

"Had they known what was about to happen, they would have changed their plans."
(((cf perf) they.pro 
            (know.v (the.d (what.rel 
                             ((past be.v) 
                              (about_to.adv-s happen.v)))))) 
 (they.pro ((cf will.aux-s) (perf (change.v (their.d (plur plan.n))))))) 
;; v2. -- using ans-to rather than relativizer.
(((cf perf) they.pro 
            (know.v
              (ans-to (what.pro ((past be.v) (about_to.adv-s happen.v)))))) 
 (they.pro ((cf will.aux-s) (perf (change.v (their.d (plur plan.n))))))) 

"Had he taken his doctor's advice, he might not have died."
(((cf perf) he.pro (take.v (((his.d doctor.n) 's) advice.n))) 
    (he.pro ((cf might.aux-s) not.adv-s (perf die.v))))  

"Had I arrived earlier, I could have seen Kelly."
(((cf perf) I.pro (arrive.v earlier.adv-e))
    (I.pro ((cf can.aux-s) (perf (see.v |Kelly|))))) 

"Were I you, I would ignore it."
(((cf Were.v) I.pro (= you.pro)) 
 (I.pro ((cf will.aux-s) (ignore.v it.pro)))) 

"Were I you, I would follow his advice."
(((cf Were.v) I.pro (= you.pro)) 
 (I.pro ((cf will.aux-s) (follow.v (his.d advice.n))))) 

"Had he known the facts, the accident might have been avoided."
(((cf perf) he.pro (know.v (the.d (plur fact.n)))) 
 ((the.d accident.n) ((cf might.aux-s) (perf (pasv avoid.v))))) 

"Had he known the truth, he would have told me."
(((cf perf) he.pro (know.v (the.d truth.n))) 
    (he.pro ((cf will.aux-s) (perf (tell.v me.pro))))) 

"Were I a bird, I would fly to you."
(((cf Were.v) I.pro (= (a.d bird.n))) 
 (I.pro ((cf will.aux-s) (fly.v (adv-a (to.p you.pro)))))) 

"Were I in your position, I would oppose that plan."
(((cf Were.v) I.pro (in.p (your.d position.n))) 
 (I.pro ((cf will.aux-s) (oppose.v (that.d plan.n))))) 

"Had I realized what you intended, I would not have agreed."
(((cf perf) 
  I.pro 
  (realize.v
    (the.d (sub what.rel 
                (you.pro ((past intend.v) *h)))))) 
 (I.pro ((cf will.aux-s) not.adv-s (perf agree.v)))) 
;; v2. -- ans-to instead of relativizer.
(((cf perf) 
  I.pro (realize.v
          (ans-to (what.pro 
                    (you.pro ((past intend.v) *h)))))) 
 (I.pro ((cf will.aux-s) not.adv-s (perf agree.v)))) 

"Had I known about it, I would have told you."
(((cf perf) I.pro (know.v (about.p-arg it.pro)))
 (I.pro ((cf will.aux-s) (perf (tell.v you.pro))))) 

"Were I in your place I would do the same thing."
(((cf Were.v) I.pro (in.p (your.d place.n))) 
 (I.pro ((cf will.aux-s) (do.v (the.d (same.a thing.n)))))) 

"Were I in your position, I would do it at once."
(((cf Were.v) I.pro (in.pro (your.d position.n)))
 (I.pro (((cf will.aux-s) (do.v it.pro)) at_once.adv-s))) 

"Were I free from work, I could read these books."
(((cf Were.v) I.pro (free.a (adv-a (from.p (k work.n)))))
 (I.pro ((cf can.aux-v) (read.v (these.d (plur book.n)))))) 

"Were you to know the fact you would be surprised."
(((cf Were.v) you.pro (to (know.v (the.d fact.n)))) 
 (you.pro ((cf will.aux-s) (be.v surprised.a)))) 
;;
;; Requests
;;

"Could you call again later, please?"
((((pres Could.aux-v) you.pro ((call.v again.adv-a) later.adv-e))  
  please.adv-s) ?)

"Can you imagine what our lives would be like without electricity?"
(((pres Can.aux-v) you.pro 
  (imagine.v (the.d (sub what.rel 
                         ((our.d (plur life.n)) 
                            ((cf will.aux-s) 
                                ((be.v (like.a *h))
                                 (adv-e (without.p (k electricity.n)))))))))) ?)
;; v2. -- ans-to instead of relativizer.
(((pres Can.aux-v) 
  you.pro 
  (imagine.v (ans-to (sub what.pro 
                          ((our.d (plur life.n)) 
                           ((cf will.aux-s) 
                            ((be.v (like.a *h))
                             (adv-e (without.p (k electricity.n)))))))))) ?)

"Can I stay at your place?"
(((pres Can.aux-v) I.pro
  (stay.v (adv-a (at.p (your.d place.n))))) ?)

"Could you turn on the light please?"
((((pres Could.aux-v)
   you.pro
   (turn_on.v (the.d light.n))) please.adv-s) ?)

"Could you do me a favour please?"
((((pres Could.aux-v) you.pro (do.v me.v (a.d favour.n))) please.adv-s) ?)

"Can you justify the use of violence?"
(((pres Can.aux-v) you.pro (justify.v (the.d (use-of.n (k violence.n))))) ?)

"Can you do bookkeeping?"
(((pres Can.aux-v) you.pro (do.v (ka bookkeep.v))) ?)

"Can you keep a secret?"
(((pres Can.aux-v) you.pro (keep.v (a.d secret.n))) ?)

"Can you ride a horse?"
(((pres Can.aux-v) you.pro (ride.v (a-gen.d horse.n))) ?)

"Can you tell wheat from barley?"
(((pres Can.aux-v) you.pro
  ((tell.v (k wheat.n))
     (adv-a (from.p (k barley.n))))) ?)

"Can you throw a fastball?"
(((pres Can.aux-v) you.pro (throw.v (a.d fastball.n))) ?)

"Can you eat raw oysters?"
(((pres Can.aux-v) you.pro (eat.v (k (raw.a (plur oyster.n))))) ?)

"Can you break away from your parents?"
(((pres Can.aux-v) you.pro
  (break_away.v (from.p-arg (your.d (plur (parent-of.n *s)))))) ?)
;;
;; Wishes.
;;

"I wish I could care more about my grades but it seems that, at a certain point of my life, I decided they would not be so important anymore."
((I.pro 
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

"What famous song do you wish you had composed, and why?"
((sub (What.d (famous.a (plur song.n))) 
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

"I wish that you had told me the truth."
(I.pro ((pres wish.v)
           (tht 
             (you.pro ((cf perf) (tell.v me.pro (the.d truth.n))))))) 

"You will wish you had never seen it."
(You.pro
  ((pres will.aux-s)
     (wish.v
       (tht
         (you.pro ((cf perf) never.adv-e (see.v it.pro))))))) 

"I wish you had told me the truth then."
(I.pro
   ((pres wish.v)
     (tht
       (you.pro 
         ((cf perf) ((tell.v me.pro (the.d truth.n)) then.adv-e)))))) 

"Your wish will come true in the near future."
((Your.d wish.n)
    ((pres will.aux-s) 
       (come_true.v (adv-e (in.p (the.d (near.a future.n))))))) 

"I wish I could live near your house."
(I.pro 
    ((pres wish.v)
       (tht
         (I.pro 
           ((cf could.aux-v) (live.v (adv-a (near.p (your.d house.n))))))))) 

"I wish I could go to the part with you."
(I.pro
   ((pres wish.v)
      (tht
        (I.pro
          ((cf could.aux-v)
            (go.v (adv-a (to.p (the.d part.n)))
                  (adv-a (with.p you.pro)))))))) 

"I wish you could come with us."
(I.pro
   ((pres wish.v)
      (tht
        (you.pro ((cf could.aux-v) (come.v (adv-a (with.p us.pro)))))))) 

"I wish you could drop in at my house on your way home."
(I.pro
   ((pres wish.v)
      (tht
        (you.pro
          ((cf could.aux-v)
            (drop_in.v (adv-a (at.p (my.d house.n)))
                       (adv-a (on.p (your.d (n+preds way.n
                                                     ({toward}.p (k home.n)))))))))))) 

"I wish you were close to me."
(I.pro
   ((pres wish.v)
     (tht
       (you.pro 
         ((cf were.v) (close.a (to.p-arg me.pro)))))))

"I wish you had not told the story to my mother."
(I.pro
  ((pres wish.v)
    (tht
      (you.pro
        ((cf perf) not.adv-s (tell.v (the.d story.n) (to.p-arg (my.d (mother-of.n *s))))))))) 

"I wish I were rich."
(I.pro
  ((pres wish.v)
    (tht
      (I.pro ((cf were.v) rich.a))))) 

"I wish our classroom were air-conditioned."
(I.pro
  ((pres wish.v)
    (tht
      ((our.d classroom.n) ((cf were.v) air-conditioned.a))))) 

