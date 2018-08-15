;; Text file that simply lists all the generally sampled formulas.
;; This is meant to be loaded into Lisp with the 'read' function.

"They are sensible girls."
(They.pro ((pres be.v) (sensible.a (plur girl.n)))) 

"You are not better at remembering things that I am."
(You.pro ((pres be.v) 
    not 
    (more-x-than 
             ((degree1 good.a) 
              (adv-a (at.p (ka (remember.v (plur thing.n)))))) 
    (I.pro (pres be.v) 
             {((degree2 good.a) 
               (adv-a (at.p (ka (remember.v (plur thing.n))))))})))) 

"You must work more."
(You.pro ((pres must.aux-s) (work.v more.adv-a))) 

"I dod not agree with him."
(I.pro ((pres do.aux-s) not (agree.v (with-arg.p him.pro)))) 

"The spirit is willing, but the flesh is weak."
(((The-gen.d spirit.n) ((pres be.v) willing.a))  but.cc 
 ((the-gen.d flesh.n) ((pres be.v) weak.a))) 

"How beautiful you are!"
(sub (How.adv-a beautiful.a) 
     (you.pro ((pres be.v) *h)))

"It is easier to have fun than to work."
;(It.pro ((pres be.v) 
;     (more-x-than 
;          ((degree1 easy.a) (adv-a ({for}.p (to (have.v (k fun.n)))))) 
;          ({It.pro ((pres be.v) ((degree2 easy.a)} (adv-a (to.p (to work.v)))))))))
;; Added extraposition handling...
;; Still unsure so the original is above.
;; more-x-than is preferable for word order, but doesn't mix with it-extra handling...
((It-extra.pro (((pres be.v) (degree1 easy.a))
                (to (have.v (k fun.n)))))
 more-than
 ((to work.v) {((pres be.v) (degree2 easy.a))}))

"It is more difficult than you think."
(It.pro ((pres be.v) 
   (more-x-than 
      (degree1 difficult.a)
      (you.pro 
         ((pres think.v)
             {(tht (it.pro ((pres be.v) (degree2 difficult.a))))}))))) 

"He told me he would go to Venice."
(He.pro ((past tell.v) 
          me.pro 
          (tht (he.pro ((past will.aux-s) 
                        (go.v (adv-a (to.p |Venice|)))))))) 

"Go and speak to my colleague."
(({you.pro} (Go.v and.cc
             (speak.v (to.p-arg (my.d (colleague-of.n *s)))))) !) 

"Who are those guys?"
((sub Who.pro ((pres be.v) (those.d (plur guy.n)) (= *h))) ?)

"Tell me about it!"
(({you.pro} (((pres Tell.v) me.pro) (about.p-arg it.pro))) !)

"Let us not be in too much of a hurry."
(({you}.pro 
    ((pres Let.v) us.pro 
     (to (non- (be.v (in.p (k ((too.adv-a much.a) 
                               (of.p (a.d hurry.n)))))))))) !) 

"In which folder did you save the file?"
((sub (In.p (which.d folder.n))
      ((past do.aux-s) you.pro ((save.v (the.d file.n))
                                  (adv-a *h)))) ?)

"It seems to me that the train is late."
(It.pro 
  ((pres seem.v) 
     (to-arg.p me.pro) 
     (that ((the.d train.n) ((pres be.v) late.a))))) 

"He learned the news while reading the newspaper."
(He.pro 
   (((past learn.v) (the.d news.n)) 
        (while.adv-a (ka (read.v (the-gen.d newspaper.n)))))) 

"Maria has long hair."
(|Maria| ((pres have.v) (k (long.a hair.n)))) 

"I do not know if I will have time to do it."
(I.pro 
  ((pres do.aux-s) not 
   (know.v 
     (whether (I.pro ((pres will.aux-s) 
                      (have.v (k (n+preds time.n
                                          ({for}.p (to (do.v it.pro)))))))))))) 

"You do not have to come tomorrow."
(You.pro 
  ((pres do.aux-s) not 
   (have.v (to (come.v tomorrow.adv-e))))) 

"Ask Trang if he is going out this evening."
(({you}.pro
  (Ask.v 
    |Trang| 
    (whether 
      (he.pro ((pres prog) (go_out.v (adv-e (this.a evening.n)))))))) !) 

"I have to take medicine."
(I.pro 
    ((pres have.v) 
       (to (take.v (k medicine.n))))) 

"Please do not take photos here."
((Please.adv-s 
   ({you}.pro
     (((pres do.aux-s) not (take.v (k (plur photo.n)))) 
      here.adv-e))) !) 

"She has got a fair complexino while her brother is very dark."
((She.pro 
   ((pres have_got.v) (a.d (fair.a complexion.n))))
  while-whereas.cc 
  ((her.d brother.n) ((pres be.v) (very.adv-a dark.a)))) 

"In a town you may pass unnoticed whereas in a village it is impossible."
(((adv-e (In.p (a-gen.d town.n)))
    (you.pro ((pres may.aux-s) (pass.v (adv-a unnoticed.a)))))  
  whereas.cc 
  ((adv-e (in.p (a-gen.d village.n))) 
     (it.pro ((pres be.v) impossible.a)))) 

"Nara is a quiet and beautiful city."
(|Nara| ((pres be.v) (= (a.d ((quiet.a and.cc beautiful.a) city.n))))) 

"Who said that?"
((Who.pro ((pres say.v) that.pro)) ?) 

"It is totally wrong!"
(It.pro ((pres be.v) (totally.adv-a wrong.a)))

"I am taking a walk in a park."
(I.pro 
   (((pres prog) (take.v (a.d walk.n))) 
      (adv-e (in.p (a.d park.n))))) 

"When I was a child, I would spend hours reading alone in my room."
((When.ps (I.pro ((past be.v) (= (a.d child.n)))))  
 (I.pro 
   ((past will.aux-s) 
    ((spend.v (k (plur hour.n)) 
              (ka ((read.v alone.adv-a) 
                   (adv-e (in.p (my.d room.n)))))))))) 

"My hobby is to listen to music."
((My.d hobby.n) 
    ((pres be.v) 
       (= (to (listen.v 
                (to-arg.p (k music.n))))))) 

"Wolves will not usually attack people."
((k (plur Wolf.n)) 
  ((pres will.aux-s) not usually.adv-f 
     (attack.v (k (plur person.n))))) 

"If you are free, give me a hand."
((If.ps (you.pro ((pres be.v) free.a))) 
 (({you}.pro (give.v me.pro (a.d hand.n))) !)) 

"Can somebody help me?"
(((pres Can.aux-v) somebody.pro (help.v me.pro)) ?)  
"I will."
(I.pro ((pres will.aux-v) {ref1}.v))  

"I work even on Sunday."
((I.pro (pres work.v)) 
 (even.adv-s (on.p |Sunday|))) 

"Please will you close the door when you go out."
((Please.adv-s 
   (((pres will.aux-v) you.pro (close.v (the.d door.n))) 
    (when.ps (you.pro (pres go_out.v))))) ?) 

"He is not working much at the moment."
(He.pro (((pres prog) not (work.v much.adv-a)) 
         (adv-e (at.p (the.d moment.n))))) 

"You have given me your cold."
(You.pro 
   ((pres perf) 
      (give.v me.pro (your.d cold.n)))) 

"It happened  a long time ago."
(It.pro ((past happen.v) (adv-e ((a.d (long.a time.n)) ago.p)))) 

"Ah!" 
Ah.x

"If I were rich, I would buy myself a house in Spain."
((If.ps (I.pro ((cf were.v) rich.a))) 
 (I.pro ((cf will.aux-s) 
         (buy.v myself.pro (a.d (n+preds house.n 
                                         (in.p |Spain|))))))) 

"Where have you been?"
((sub Where.pq ((pres perf) you.pro (be.v *h))) ?)

"Here comes the bride!"
(Here.adv-e ((the.d bride.n) come.v))

"I hope he will be able to come!"
(I.pro 
  ((pres hope.v) 
   (tht
     (he.pro ((pres will.aux-s) 
              (be.v (able.a (to come.v)))))))) 
"I would like to see him."
(I.pro ((pres would.aux-s) 
           (like.v (to (see.v him.pro))))) 

"What have you been doing all this time!"
(sub What.pro 
      (((pres perf) 
        you.pro 
        (prog (do.v *h))) 
         (adv-e (all.a (this.a time.n)))))

"They will never accept; it is too far."
((They.pro ((pres will.aux-s) never.adv-f (accept.v {ref}.pro)))  
  (it.pro ((pres be.v) (too.adv-a far.a)))) 

"It has been snowing all night."
((It.pro ((pres perf) (prog snow.v))) (adv-e (all.a night.n))) 

"Her garden is a work of art."
((Her.d garden.n) 
 ((pres be.v) (= (a.d (n+preds work.n 
                               (of.p (k art.n))))) 

"Is has been ten years since we last met."
((It.pro ((pres perf) (be.v (ten.a (plur year.n))))) 
    (since.ps (we.pro (last.adv-a (past meet.v))))) 

"If you don't want to stay alone, I can keep you company."
((If.ps (you.pro ((pres do.aux-s) not (want.v (to (stay.v alone.adv-a)))))) 
  (I.pro ((pres can.aux-v) (keep.v you.pro (k company.n))))) 

"Water freezes at zero degrees Celcius, doesn't it?"
(((k Water.n) 
  ((pres freeze.v) 
   (adv-a (at.p (ds temp "zero degrees Celcius"))))) .?)

"How come you know so much about Japanese history?"
((How_come.adv-s 
   (you.pro 
     (((pres know.v) 
        (adv-a (so.adv-a much.a))) 
          (adv-a (about.p (|Japanese|.a history.n)))))) ?)

"If you take care of the small things, the big things will take care of themselves."
((If.ps 
   (you.pro ((pres take.v) 
             (k (n+preds care.n 
                         (of.p (the.d (small.a (plur thing.n))))))))) 
  ((the.d (big.a (plur thing.n))) 
     ((pres will.aux-s) 
        (take.v (k (n+preds care.n (of.p themselves.pro))))))) 

"Could you turn on the light please?"
(((pres Could.aux-v) you.pro 
  (turn.v on.adv-a (the.d light.n)) please.adv-s) ?)

"She is selling drugs at the concert."
(She.pro 
   (((pres prog) (sell.v (k (plur drug.n)))) 
      (adv-a (at.p (k (plur concert.n)))))) 

"When we seek to discover the best in others, we somehow bring out the best in ourselves."
((When.ps 
    (we.pro ((pres seek.v) 
               (to (discover.v 
                      (the.d (n+preds (best.a {ref1}.n)
                                      (in.p others.pro)))))))) 
 (we.pro somehow.adv-s 
   ((pres bring_out.v) 
      (the.d (n+preds (best.a {ref1}.n) 
                      (in.p ourselves.pro)))))) 

"Turn right at the crossroad."
(({you}.pro (Turn.v right.adv-a 
                    (adv-a (at.p (the.d crossroad.n))))) !) 

"Forget it."
(({you}.pro (Forget.v it.pro)) !)  

"It isn't worth it."
(It.pro ((pres be.v) not (worth.a it.pro))) 

"I buried my dog at the pet cemetery."
((I.pro 
  (((past bury.v) (my.d dog.n)) 
     (adv-e (at.p (the.d (pet.n cemetery.n)))))) 

"We are going to eat a lot tonight so I hope you are not on a diet."
((We.pro 
    ((pres going_to.v) 
     (eat.v (a.d (lot-of.n {ref1}.pro)))
     tonight.adv-e)) so.cc 
 (I.pro 
   ((pres hope.v) 
      (tht (you.pro 
             ((pres be.v) not 
                (on.p (a.d diet.n)))))))) 

"They forgot to lock the door."
(They.pro 
   ((past forget.v) 
      (to (lock.v (the.d door.n))))) 

"Even plastic surgery will not do anything for your ugliness."
((Even.adv-s (k (plastic.a surgery.n))) 
 ((pres will.aux-s) not ((do.v anything.pro) 
                         (adv-a (for.p (your.d ugliness.n)))))) 

"You need not telephone me."
(You.pro 
  ((pres need.aux-v) not 
                     (telephone.v me.pro))) 

"Does it also work without registration?"
(((pres Do.aux-s) it.pro also.adv-s (work.v (adv-a (without.p (k registration.n))))) ?)

"There are quite a lot of things to do; do you want some help?"
((There.pro 
  ((pres be.v) 
    ((fquan (quite.adv-a (a.a lot_of.a)))
     (n+preds (plur thing.n) 
              ({for}.p (to do.v))))))  
 (((pres do.aux-s) you.pro (want.v (some.d help.n))) ?))

"I heard a cotton candy shop has just opened."
(I.pro ((past hear.v) 
            (tht ((a.d ((cotton.n candy.n) shop.n)) 
                  ((pres perf) (just.adv-e open.v))))))  

"Let's go, dudes."
((({you}.pro 
   (Let.v us.pro (ka go.v)))  
  (voc (plur dude.n))) !) 

"We have to take him to the hospital immediately; he is seriously injured!"
((We.pro ((pres have.v) 
      (to (((take.v him.pro) 
            (adv-a (to.p (the.d hospital.n)))) 
          immediately.adv-e))))  
  (he.pro ((pres be.v) (seriously.adv-a injured.a))))

"I still have a few things I need to do."
((I.pro still.adv-s 
  ((pres have.v) 
     ((fquan (a.a few.a)) 
        (n+preds (plur thing.n) 
                 (sub tht.rel 
                      (I.pro ((pres need.v) (to (do.v *h))))))))) 

