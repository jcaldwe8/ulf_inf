;; Gene Kim 8-24-2018
;; Test cases based on formulas from the pilot inference system's evaluation.
;; This file contains just the sentences that were sampled in general from the
;; dataset.

;; Macro for running a test to reduce the size of the test declarations.
;; Arguments
;;  name:     The name of the test (as a symbol, i.e. unquoted)
;;  sentence: String surface sentence being tested
;;  ulf:      Source ULF
;;  expected: List of expected output ULFs (in order TODO: make the order not
;;            matter)
(defmacro define-pilot-general-sampled-test (name sentence tags ulf expected)
  `(define-test ,name
     ,(format nil "Pilot experiment select sampled sentence '~a'" sentence) 
     (:tag :infer-all :general-pilot ,@tags)
     (let ((actual (mapcar #'result-formula (infer-all ,ulf)))
           (expected ,expected))
        (set-assert-equal actual expected))))

;;
;; Tests.
;;

(define-pilot-general-sampled-test 
  pilot-general-sampled-1
  ()
  "They are sensible girls."
  '(They.pro ((pres be.v) (sensible.a (plur girl.n))))
  '())
  
(define-pilot-general-sampled-test 
  pilot-general-sampled-2
  (:comparative)
  "You are not better at remembering things than I am."
  '(You.pro ((pres be.v) 
      not 
      (more-x-than 
               ((degree1 good.a) 
                (adv-a (at.p (ka (remember.v (plur thing.n)))))) 
      (I.pro (pres be.v) 
               {((degree2 good.a) 
                 (adv-a (at.p (ka (remember.v (plur thing.n))))))})))) 
  '())
  
(define-pilot-general-sampled-test 
  pilot-general-sampled-3
  ()
  "You must work more."
  '(You.pro ((pres must.aux-s) (work.v more.adv-a))) 
  '())
  
(define-pilot-general-sampled-test 
  pilot-general-sampled-4
  ()
  "I do not agree with him."
  '(I.pro ((pres do.aux-s) not (agree.v (with.p-arg him.pro)))) 
  '())
  
(define-pilot-general-sampled-test 
  pilot-general-sampled-5
  ()
  "The spirit is willing, but the flesh is weak."
  '(((The-gen.d spirit.n) ((pres be.v) willing.a)) but.cc 
   ((the-gen.d flesh.n) ((pres be.v) weak.a))) 
  '())
  
(define-pilot-general-sampled-test 
  pilot-general-sampled-6
  ()
  "How beautiful you are!"
  '(sub (How.adv-a beautiful.a) 
       (you.pro ((pres be.v) *h)))
  '())
  
(define-pilot-general-sampled-test 
  pilot-general-sampled-7
  (:comparative)
  "It is easier to have fun than to work."
  ;(It.pro ((pres be.v) 
  ;     (more-x-than 
  ;          ((degree1 easy.a) (adv-a ({for}.p (to (have.v (k fun.n)))))) 
  ;          ({It.pro ((pres be.v) ((degree2 easy.a)} (adv-a (to.p (to work.v)))))))))
  ;; Added extraposition handling...
  ;; Still unsure so the original is above.
  ;; more-x-than is preferable for word order, but doesn't mix with it-extra handling...
  '((It-extra.pro (((pres be.v) (degree1 easy.a))
                  (to (have.v (k fun.n)))))
   more-than
   ((to work.v) {((pres be.v) (degree2 easy.a))}))
  '())
  
(define-pilot-general-sampled-test 
  pilot-general-sampled-8
  (:comparative)
  "It is more difficult than you think."
  '(It.pro ((pres be.v) 
     (more-x-than 
        (degree1 difficult.a)
        (you.pro 
           ((pres think.v)
               {(tht (it.pro ((pres be.v) (degree2 difficult.a))))}))))) 
  '())
  
(define-pilot-general-sampled-test 
  pilot-general-sampled-9
  ()
  "He told me he would go to Venice."
  '(He.pro ((past tell.v) 
            me.pro 
            (tht (he.pro ((past will.aux-s) 
                          (go.v (adv-a (to.p |Venice|)))))))) 
  '())
  
(define-pilot-general-sampled-test 
  pilot-general-sampled-10
  (:imperative)
  "Go and speak to my colleague."
  '(({you.pro} ((pres Go.v) and.cc
               ((pres speak.v) (to.p-arg (my.d (colleague-of.n *s)))))) !) 
  '())
  
(define-pilot-general-sampled-test 
  pilot-general-sampled-11
  (:question)
  "Who are those guys?"
  '((sub Who.pro ((pres be.v) (those.d (plur guy.n)) (= *h))) ?)
  '())
  
(define-pilot-general-sampled-test 
  pilot-general-sampled-12
  (:imperative)
  "Tell me about it!"
  '(({you.pro} (((pres Tell.v) me.pro) (about.p-arg it.pro))) !)
  '())
  
(define-pilot-general-sampled-test 
  pilot-general-sampled-13
  (:imperative)
  "Let us not be in too much of a hurry."
  '(({you}.pro 
      ((pres Let.v) us.pro 
       (to (non- (be.v (in.p (k ((too.adv-a much.a) 
                                 (of.p (a.d hurry.n)))))))))) !) 
  '())
  
(define-pilot-general-sampled-test 
  pilot-general-sampled-14
  (:question)
  "In which folder did you save the file?"
  '((sub (In.p (which.d folder.n))
        ((past do.aux-s) you.pro ((save.v (the.d file.n))
                                    (adv-a *h)))) ?)
  '())
  
(define-pilot-general-sampled-test 
  pilot-general-sampled-15
  ()
  "It seems to me that the train is late."
  '(It.pro 
    ((pres seem.v) 
       (to.p-arg me.pro) 
       (that ((the.d train.n) ((pres be.v) late.a))))) 
  '())
  
(define-pilot-general-sampled-test 
  pilot-general-sampled-16
  ()
  "He learned the news while reading the newspaper."
  '(He.pro 
     (((past learn.v) (the.d news.n)) 
          (while.adv-a (ka (read.v (the-gen.d newspaper.n)))))) 
  '())
  
(define-pilot-general-sampled-test 
  pilot-general-sampled-17
  ()
  "Maria has long hair."
  '(|Maria| ((pres have.v) (k (long.a hair.n)))) 
  '())
  
(define-pilot-general-sampled-test 
  pilot-general-sampled-18
  ()
  "I do not know if I will have time to do it."
  '(I.pro 
    ((pres do.aux-s) not 
     (know.v 
       (whether (I.pro ((pres will.aux-s) 
                        (have.v (k (n+preds time.n
                                            ({for}.p (to (do.v it.pro)))))))))))) 
  '())
  
(define-pilot-general-sampled-test 
  pilot-general-sampled-19
  ()
  "You do not have to come tomorrow."
  '(You.pro 
    ((pres do.aux-s) not 
     (have.v (to (come.v tomorrow.adv-e))))) 
  '())
  
(define-pilot-general-sampled-test 
  pilot-general-sampled-20
  (:imperative)
  "Ask Trang if he is going out this evening."
  '(({you}.pro
    (Ask.v 
      |Trang| 
      (whether 
        (he.pro ((pres prog) (go_out.v (adv-e (this.a evening.n)))))))) !) 
  '())
  
(define-pilot-general-sampled-test 
  pilot-general-sampled-21
  ()
  "I have to take medicine."
  '(I.pro 
    ((pres have.v) 
     (to (take.v (k medicine.n))))) 
  '())
  
(define-pilot-general-sampled-test 
  pilot-general-sampled-22
  (:imperative)
  "Please do not take photos here."
  '((Please.adv-s 
     ({you}.pro
       (((pres do.aux-s) not (take.v (k (plur photo.n)))) 
        here.adv-e))) !) 
  '())
  
(define-pilot-general-sampled-test 
  pilot-general-sampled-23
  ()
  "She has got a fair complexion while her brother is very dark."
  '((She.pro 
     ((pres have_got.v) (a.d (fair.a complexion.n))))
    while-whereas.cc 
    ((her.d brother.n) ((pres be.v) (very.adv-a dark.a)))) 
  '())
  
(define-pilot-general-sampled-test 
  pilot-general-sampled-24
  ()
  "In a town you may pass unnoticed whereas in a village it is impossible."
  '(((adv-e (In.p (a-gen.d town.n)))
      (you.pro ((pres may.aux-s) (pass.v (adv-a unnoticed.a)))))  
    whereas.cc 
    ((adv-e (in.p (a-gen.d village.n))) 
       (it.pro ((pres be.v) impossible.a)))) 
  '())
  
(define-pilot-general-sampled-test 
  pilot-general-sampled-25
  ()
  "Nara is a quiet and beautiful city."
  '(|Nara| ((pres be.v) (= (a.d ((quiet.a and.cc beautiful.a) city.n))))) 
  '())
  
(define-pilot-general-sampled-test 
  pilot-general-sampled-26
  (:question)
  "Who said that?"
  '((Who.pro ((pres say.v) that.pro)) ?) 
  '())
  
(define-pilot-general-sampled-test 
  pilot-general-sampled-27
  ()
  "It is totally wrong!"
  '(It.pro ((pres be.v) (totally.adv-a wrong.a)))
  '())
  
(define-pilot-general-sampled-test 
  pilot-general-sampled-28
  ()
  "I am taking a walk in a park."
  '(I.pro 
     (((pres prog) (take.v (a.d walk.n))) 
        (adv-e (in.p (a.d park.n))))) 
  '())
  
(define-pilot-general-sampled-test 
  pilot-general-sampled-29
  ()
  "When I was a child, I would spend hours reading alone in my room."
  '((When.ps (I.pro ((past be.v) (= (a.d child.n)))))  
   (I.pro 
     ((past will.aux-s) 
      ((spend.v (k (plur hour.n)) 
                (ka ((read.v alone.adv-a) 
                     (adv-e (in.p (my.d room.n)))))))))) 
  '())
  
(define-pilot-general-sampled-test 
  pilot-general-sampled-30
  ()
  "My hobby is to listen to music."
  '((My.d hobby.n) 
      ((pres be.v) 
         (= (to (listen.v 
                  (to.p-arg (k music.n))))))) 
  '())
  
(define-pilot-general-sampled-test 
  pilot-general-sampled-31
  ()
  "Wolves will not usually attack people."
  '((k (plur Wolf.n)) 
    ((pres will.aux-s) not usually.adv-f 
       (attack.v (k (plur person.n))))) 
  '())
  
(define-pilot-general-sampled-test 
  pilot-general-sampled-32
  (:if-then)
  "If you are free, give me a hand."
  '((If.ps (you.pro ((pres be.v) free.a))) 
   (({you}.pro (give.v me.pro (a.d hand.n))) !)) 
  '())
  
(define-pilot-general-sampled-test 
  pilot-general-sampled-33
  (:question)
  "Can somebody help me?"
  '(((pres Can.aux-v) somebody.pro (help.v me.pro)) ?)  
  '((I.pro ((pres want.v) somebody.pro (to (help.v me.pro))))
    (I.pro ((pres expect.v) (that (somebody.pro ((pres help.v) me.pro)))))))

(define-pilot-general-sampled-test 
  pilot-general-sampled-34
  ()
  "I will."
  '(I.pro ((pres will.aux-v) {ref1}.v))  
  '())
  
(define-pilot-general-sampled-test 
  pilot-general-sampled-35
  ()
  "I work even on Sunday."
  '((I.pro (pres work.v)) 
   (even.adv-s (on.p |Sunday|))) 
  '())
  
(define-pilot-general-sampled-test 
  pilot-general-sampled-36
  (:request)
  "Please will you close the door when you go out."
  '((Please.adv-s 
     (((pres will.aux-v) you.pro ((close.v (the.d door.n)) 
                                  (when.ps (you.pro (pres go_out.v))))))) ?) 
  '((I.pro ((pres want.v) you.pro 
            (to ((close.v (the.d door.n)) 
                 (when.ps (you.pro (pres go_out.v)))))))
    (I.pro ((pres expect.v)
            (that (you.pro (((pres close.v) (the.d door.n))
                           (when.ps (you.pro (pres go_out.v))))))))))
  
(define-pilot-general-sampled-test 
  pilot-general-sampled-37
  ()
  "He is not working much at the moment."
  '(He.pro (((pres prog) not (work.v much.adv-a)) 
           (adv-e (at.p (the.d moment.n))))) 
  '())
  
(define-pilot-general-sampled-test 
  pilot-general-sampled-38
  ()
  "You have given me your cold."
  '(You.pro 
     ((pres perf) 
        (give.v me.pro (your.d cold.n)))) 
  '())
  
(define-pilot-general-sampled-test 
  pilot-general-sampled-39
  ()
  "It happened  a long time ago."
  '(It.pro ((past happen.v) (adv-e ((a.d (long.a time.n)) ago.p)))) 
  '())
  
(define-pilot-general-sampled-test 
  pilot-general-sampled-40
  ()
  "Ah!" 
  'Ah.x
  '())
  
(define-pilot-general-sampled-test 
  pilot-general-sampled-41
  (:if-then)
  "If I were rich, I would buy myself a house in Spain."
  '((If.ps (I.pro ((cf were.v) rich.a))) 
   (I.pro ((cf will.aux-s) 
           (buy.v myself.pro (a.d (n+preds house.n 
                                           (in.p |Spain|))))))) 
  '((I.pro ((pres be.v) not.adv-s rich.a))
    (I.pro ((pres will.aux-s) not.adv-s
            (buy.v myself.pro (a.d (n+preds house.n (in.p |Spain|))))))))
  
(define-pilot-general-sampled-test 
  pilot-general-sampled-42
  (:question)
  "Where have you been?"
  '((sub Where.pq ((pres perf) you.pro (be.v *h))) ?)
  '())
  
(define-pilot-general-sampled-test 
  pilot-general-sampled-43
  ()
  "Here comes the bride!"
  '(Here.adv-e ((the.d bride.n) come.v))
  '())
  
(define-pilot-general-sampled-test 
  pilot-general-sampled-44
  ()
  "I hope he will be able to come!"
  '(I.pro 
    ((pres hope.v) 
     (tht
       (he.pro ((pres will.aux-s) 
                (be.v (able.a (to come.v)))))))) 
  '())
(define-pilot-general-sampled-test 
  pilot-general-sampled-45
  ()
  "I would like to see him."
  '(I.pro ((pres would.aux-s) 
             (like.v (to (see.v him.pro))))) 
  '())
  
(define-pilot-general-sampled-test 
  pilot-general-sampled-46
  ()
  "What have you been doing all this time!"
  '((sub What.pro 
        (((pres perf) 
          you.pro 
          (prog (do.v *h))) 
           (adv-e (all.a (this.a time.n))))) ?)
  '())
  
(define-pilot-general-sampled-test 
  pilot-general-sampled-47
  ()
  "They will never accept; it is too far."
  '((They.pro ((pres will.aux-s) never.adv-f (accept.v {ref}.pro)))  
   (it.pro ((pres be.v) (too.adv-a far.a)))) 
  '())
  
(define-pilot-general-sampled-test 
  pilot-general-sampled-48
  ()
  "It has been snowing all night."
  '((It.pro ((pres perf) (prog snow.v))) (adv-e (all.a night.n))) 
  '())
  
(define-pilot-general-sampled-test 
  pilot-general-sampled-49
  ()
  "Her garden is a work of art."
  '((Her.d garden.n) 
   ((pres be.v) (= (a.d (n+preds work.n 
                                 (of.p (k art.n))))))) 
  '())

(define-pilot-general-sampled-test 
  pilot-general-sampled-50
  ()
  "It has been ten years since we last met."
  '((It.pro ((pres perf) (be.v (ten.a (plur year.n))))) 
      (since.ps (we.pro (last.adv-a (past meet.v))))) 
  '())
  
(define-pilot-general-sampled-test 
  pilot-general-sampled-51
  (:if-then)
  "If you don't want to stay alone, I can keep you company."
  '((If.ps (you.pro ((pres do.aux-s) not (want.v (to (stay.v alone.adv-a)))))) 
    (I.pro ((pres can.aux-v) (keep.v you.pro (k company.n))))) 
  '())
  
(define-pilot-general-sampled-test 
  pilot-general-sampled-52
  (:question :tag-question)
  "Water freezes at zero degrees Celcius, doesn't it?"
  '(((k Water.n) 
    ((pres freeze.v) 
     (adv-a (at.p (ds temp "zero degrees Celcius"))))) .?)
  '())
  
(define-pilot-general-sampled-test 
  pilot-general-sampled-53
  (:question)
  "How come you know so much about Japanese history?"
  '((How_come.adv-s 
     (you.pro 
       (((pres know.v) 
          (adv-a (so.adv-a much.a))) 
            (adv-a (about.p (|Japanese|.a history.n)))))) ?)
  '())
  
(define-pilot-general-sampled-test 
  pilot-general-sampled-54
  (:if-then)
  "If you take care of the small things, the big things will take care of themselves."
  '((If.ps 
     (you.pro ((pres take.v) 
               (k (n+preds care.n 
                           (of.p (the.d (small.a (plur thing.n))))))))) 
    ((the.d (big.a (plur thing.n))) 
       ((pres will.aux-s) 
          (take.v (k (n+preds care.n (of.p themselves.pro))))))) 
  '())
  
(define-pilot-general-sampled-test 
  pilot-general-sampled-55
  (:question :request)
  "Could you turn on the light please?"
  '(((pres Could.aux-v) you.pro 
    (turn.v on.adv-a (the.d light.n)) please.adv-s) ?)
  '((I.pro ((pres want.v) you.pro (to (turn.v on.adv-a (the.d light.n)))))
    (I.pro ((pres expect.v) 
            (that (you.pro ((pres turn.v) on.adv-a (the.d light.n))))))))
  
(define-pilot-general-sampled-test 
  pilot-general-sampled-56
  ()
  "She is selling drugs at the concert."
  '(She.pro 
     (((pres prog) (sell.v (k (plur drug.n)))) 
        (adv-a (at.p (k (plur concert.n)))))) 
  '())
  
(define-pilot-general-sampled-test 
  pilot-general-sampled-57
  ()
  "When we seek to discover the best in others, we somehow bring out the best in ourselves."
  '((When.ps 
      (we.pro ((pres seek.v) 
                 (to (discover.v 
                        (the.d (n+preds (best.a {ref1}.n)
                                        (in.p others.pro)))))))) 
   (we.pro somehow.adv-s 
     ((pres bring_out.v) 
        (the.d (n+preds (best.a {ref1}.n) 
                        (in.p ourselves.pro)))))) 
  '())
  
(define-pilot-general-sampled-test 
  pilot-general-sampled-58
  (:imperative)
  "Turn right at the crossroad."
  '(({you}.pro (Turn.v right.adv-a 
                      (adv-a (at.p (the.d crossroad.n))))) !) 
  '())
  
(define-pilot-general-sampled-test 
  pilot-general-sampled-59
  (:imperative)
  "Forget it."
  '(({you}.pro (Forget.v it.pro)) !)  
  '())
  
(define-pilot-general-sampled-test 
  pilot-general-sampled-60
  ()
  "It isn't worth it."
  '(It.pro ((pres be.v) not (worth.a it.pro))) 
  '())
  
(define-pilot-general-sampled-test 
  pilot-general-sampled-61
  ()
  "I buried my dog at the pet cemetery."
  '((I.pro 
    (((past bury.v) (my.d dog.n)) 
       (adv-e (at.p (the.d (pet.n cemetery.n))))))) 
  '())
  
(define-pilot-general-sampled-test 
  pilot-general-sampled-62
  ()
  "We are going to eat a lot tonight so I hope you are not on a diet."
  '((We.pro 
      ((pres going_to.v) 
       (eat.v (a.d (lot-of.n {ref1}.pro)))
       tonight.adv-e)) so.cc 
   (I.pro 
     ((pres hope.v) 
        (tht (you.pro 
               ((pres be.v) not 
                  (on.p (a.d diet.n)))))))) 
  '())
  
(define-pilot-general-sampled-test 
  pilot-general-sampled-63
  (:implicative)
  "They forgot to lock the door."
  '(They.pro 
     ((past forget.v) 
        (to (lock.v (the.d door.n))))) 
  '())
  
(define-pilot-general-sampled-test 
  pilot-general-sampled-64
  ()
  "Even plastic surgery will not do anything for your ugliness."
  '((Even.adv-s (k (plastic.a surgery.n))) 
   ((pres will.aux-s) not ((do.v anything.pro) 
                           (adv-a (for.p (your.d ugliness.n)))))) 
  '())
  
(define-pilot-general-sampled-test 
  pilot-general-sampled-65
  ()
  "You need not telephone me."
  '(You.pro 
    ((pres need.aux-v) not 
                       (telephone.v me.pro))) 
  '())
  
(define-pilot-general-sampled-test 
  pilot-general-sampled-66
  (:question)
  "Does it also work without registration?"
  '(((pres Do.aux-s) it.pro also.adv-s (work.v (adv-a (without.p (k registration.n))))) ?)
  '())
  
(define-pilot-general-sampled-test 
  pilot-general-sampled-67
  (:question)
  "There are quite a lot of things to do; do you want some help?"
  '((There.pro 
    ((pres be.v) 
      ((fquan (quite.adv-a (a.a lot_of.a)))
       (n+preds (plur thing.n) 
                ({for}.p (to do.v))))))  
   (((pres do.aux-s) you.pro (want.v (some.d help.n))) ?))
  '())
  
(define-pilot-general-sampled-test 
  pilot-general-sampled-68
  ()
  "I heard a cotton candy shop has just opened."
  '(I.pro ((past hear.v) 
              (tht ((a.d ((cotton.n candy.n) shop.n)) 
                    ((pres perf) (just.adv-e open.v))))))  
  '())
  
(define-pilot-general-sampled-test 
  pilot-general-sampled-69
  (:imperative)
  "Let's go, dudes."
  '((({you}.pro 
     (Let.v us.pro (ka go.v)))  
    (voc (plur dude.n))) !) 
  '())
  
(define-pilot-general-sampled-test 
  pilot-general-sampled-70
  ()
  "We have to take him to the hospital immediately; he is seriously injured!"
  '((We.pro ((pres have.v) 
        (to (((take.v him.pro) 
              (adv-a (to.p (the.d hospital.n)))) 
            immediately.adv-e))))  
    (he.pro ((pres be.v) (seriously.adv-a injured.a))))
  '())
  
(define-pilot-general-sampled-test 
  pilot-general-sampled-71
  ()
  "I still have a few things I need to do."
  '((I.pro still.adv-s 
    ((pres have.v) 
       ((fquan (a.a few.a)) 
          (n+preds (plur thing.n) 
                   (sub tht.rel 
                        (I.pro ((pres need.v) (to (do.v *h)))))))))) 
  '())
  
