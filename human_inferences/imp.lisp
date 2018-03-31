;-------------------------------------------------------------
; List of implicatives and their lexical semantic axioms.
; Modified to handle updated ULF forms and make some finer distinctions from
; the version that Karl Stratos built. (3/18)
; TODO: figure out a better way to represent tense and progressive aspect
;
; vvv Original vvv
; List of implicatives and their lexical semantic axioms
;   -Karl Stratos (7/11) 
;-------------------------------------------------------------
;(in-package epi)

; for queries that take over 10 seconds, use my-pq and my-dq 
;(defun my-pq (q1)
;  (pq (p q1) :stop-fn-on-answers #'has-been-answered?))
;(defun my-dq (q1)
;  (dq (p q1) :stop-fn-on-answers #'has-been-answered?))

; declaration: store unknown predicates in *lexicon-kb*. 
; ^^^^^^^^^^^
;(store (x-is-predicate 'accept.v) *lexicon-kb*)
;(store (x-is-predicate 'acknowledge.v) *lexicon-kb*)
;(store (x-is-predicate 'admit.v) *lexicon-kb*)
;(store (x-is-predicate 'admonish.v) *lexicon-kb*)
;(store (x-is-predicate 'agree.v) *lexicon-kb*)
;(store (x-is-predicate 'allow.v) *lexicon-kb*)
;(store (x-is-predicate 'amaze.v) *lexicon-kb*)
;(store (x-is-predicate 'amuse.v) *lexicon-kb*)
;(store (x-is-predicate 'annoy.v) *lexicon-kb*)
;(store (x-is-predicate 'appreciate.v) *lexicon-kb*)
;(store (x-is-predicate 'ascertain.v) *lexicon-kb*)
;(store (x-is-predicate 'astonish.v) *lexicon-kb*)
;(store (x-is-predicate 'astound.v) *lexicon-kb*)
;(store (x-is-predicate 'attempt.v) *lexicon-kb*)
;(store (x-is-predicate 'baffle.v) *lexicon-kb*)
;(store (x-is-predicate 'begin.v) *lexicon-kb*)
;(store (x-is-predicate 'berate.v) *lexicon-kb*)
;(store (x-is-predicate 'bewilder.v) *lexicon-kb*)
;(store (x-is-predicate 'bother.v) *lexicon-kb*)
;(store (x-is-predicate 'care.v) *lexicon-kb*)
;(store (x-is-predicate 'cease.v) *lexicon-kb*)
;(store (x-is-predicate 'coerce.v) *lexicon-kb*)
;(store (x-is-predicate 'come.v) *lexicon-kb*)
;(store (x-is-predicate 'compel.v) *lexicon-kb*)
;(store (x-is-predicate 'concede.v) *lexicon-kb*)
;(store (x-is-predicate 'confess.v) *lexicon-kb*)
;(store (x-is-predicate 'confirm.v) *lexicon-kb*)
;(store (x-is-predicate 'confuse.v) *lexicon-kb*)
;(store (x-is-predicate 'continue.v) *lexicon-kb*)
;(store (x-is-predicate 'convince.v) *lexicon-kb*)
;(store (x-is-predicate 'dare.v) *lexicon-kb*)
;(store (x-is-predicate 'decline.v) *lexicon-kb*)
;(store (x-is-predicate 'delight.v) *lexicon-kb*)
;(store (x-is-predicate 'demonstrate.v) *lexicon-kb*)
;(store (x-is-predicate 'deplore.v) *lexicon-kb*)
;(store (x-is-predicate 'depress.v) *lexicon-kb*)
;(store (x-is-predicate 'detest.v) *lexicon-kb*)
;(store (x-is-predicate 'disappoint.v) *lexicon-kb*)
;(store (x-is-predicate 'disconcert.v) *lexicon-kb*)
;(store (x-is-predicate 'discourage.v) *lexicon-kb*)
;(store (x-is-predicate 'discover.v) *lexicon-kb*)
;(store (x-is-predicate 'disgust.v) *lexicon-kb*)
;(store (x-is-predicate 'disillusion.v) *lexicon-kb*)
;(store (x-is-predicate 'dislike.v) *lexicon-kb*)
;(store (x-is-predicate 'disregard.v) *lexicon-kb*)
;(store (x-is-predicate 'distress.v) *lexicon-kb*)
;(store (x-is-predicate 'embarrass.v) *lexicon-kb*)
;(store (x-is-predicate 'encourage.v) *lexicon-kb*)
;(store (x-is-predicate 'envy.v) *lexicon-kb*)
;(store (x-is-predicate 'excite.v) *lexicon-kb*)
;(store (x-is-predicate 'fail.v) *lexicon-kb*)
;(store (x-is-predicate 'find.v) *lexicon-kb*)
;(store (x-is-predicate 'force.v) *lexicon-kb*)
;(store (x-is-predicate 'forget.v) *lexicon-kb*)
;(store (x-is-predicate 'get.v) *lexicon-kb*)
;(store (x-is-predicate 'happen.v) *lexicon-kb*)
;(store (x-is-predicate 'hasten.v) *lexicon-kb*)
;(store (x-is-predicate 'help.v) *lexicon-kb*)
;(store (x-is-predicate 'hesitate.v) *lexicon-kb*)
;(store (x-is-predicate 'horrify.v) *lexicon-kb*)
;(store (x-is-predicate 'ignore.v) *lexicon-kb*)
;(store (x-is-predicate 'impress.v) *lexicon-kb*)
;(store (x-is-predicate 'lament.v) *lexicon-kb*)
;(store (x-is-predicate 'learn.v) *lexicon-kb*)
;(store (x-is-predicate 'lie.v) *lexicon-kb*)
;(store (x-is-predicate 'like.v) *lexicon-kb*)
;(store (x-is-predicate 'manage.v) *lexicon-kb*)
;(store (x-is-predicate 'mention.v) *lexicon-kb*)
;(store (x-is-predicate 'miss.v) *lexicon-kb*)
;(store (x-is-predicate 'neglect.v) *lexicon-kb*)
;(store (x-is-predicate 'note.v) *lexicon-kb*)
;(store (x-is-predicate 'notice.v) *lexicon-kb*)
;(store (x-is-predicate 'observe.v) *lexicon-kb*)
;(store (x-is-predicate 'outrage.v) *lexicon-kb*)
;(store (x-is-predicate 'overlook.v) *lexicon-kb*)
;(store (x-is-predicate 'permit.v) *lexicon-kb*)
;(store (x-is-predicate 'persuade.v) *lexicon-kb*)
;(store (x-is-predicate 'pity.v) *lexicon-kb*)
;(store (x-is-predicate 'please.v) *lexicon-kb*)
;(store (x-is-predicate 'praise.v) *lexicon-kb*)
;(store (x-is-predicate 'pretend.v) *lexicon-kb*)
;(store (x-is-predicate 'prohibit.v) *lexicon-kb*)
;(store (x-is-predicate 'prove.v) *lexicon-kb*)
;(store (x-is-predicate 'realize.v) *lexicon-kb*)
;(store (x-is-predicate 'recall.v) *lexicon-kb*)
;(store (x-is-predicate 'recollect.v) *lexicon-kb*)
;(store (x-is-predicate 'recognize.v) *lexicon-kb*)
;(store (x-is-predicate 'reflect.v) *lexicon-kb*)
;(store (x-is-predicate 'refrain-from.v) *lexicon-kb*)
;(store (x-is-predicate 'refuse.v) *lexicon-kb*)
;(store (x-is-predicate 'regret.v) *lexicon-kb*)
;(store (x-is-predicate 'rejoice.v) *lexicon-kb*)
;(store (x-is-predicate 'remember.v) *lexicon-kb*)
;(store (x-is-predicate 'remind.v) *lexicon-kb*)
;(store (x-is-predicate 'restrain-from.v) *lexicon-kb*)
;(store (x-is-predicate 'resume.v) *lexicon-kb*)
;(store (x-is-predicate 'reveal.v) *lexicon-kb*)
;(store (x-is-predicate 'see.v) *lexicon-kb*)
;(store (x-is-predicate 'scare.v) *lexicon-kb*)
;(store (x-is-predicate 'scold.v) *lexicon-kb*)
;(store (x-is-predicate 'shock.v) *lexicon-kb*)
;(store (x-is-predicate 'show.v) *lexicon-kb*)
;(store (x-is-predicate 'start.v) *lexicon-kb*)
;(store (x-is-predicate 'stop.v) *lexicon-kb*)
;(store (x-is-predicate 'succeed-in.v) *lexicon-kb*)
;(store (x-is-predicate 'surprise.v) *lexicon-kb*)
;(store (x-is-predicate 'suspect.v) *lexicon-kb*)
;(store (x-is-predicate 'tend.v) *lexicon-kb*)
;(store (x-is-predicate 'touch.v) *lexicon-kb*)
;(store (x-is-predicate 'trouble.v) *lexicon-kb*)
;(store (x-is-predicate 'turn.v) *lexicon-kb*)
;(store (x-is-predicate 'understand.v) *lexicon-kb*)
;(store (x-is-predicate 'unnerve.v) *lexicon-kb*)
;(store (x-is-predicate 'use.v) *lexicon-kb*)
;(store (x-is-predicate 'used-to.v) *lexicon-kb*)
;(store (x-is-predicate 'verify.v) *lexicon-kb*)
;(store (x-is-predicate 'warn.v) *lexicon-kb*)
;(store (x-is-pred-mod 'can.v) *lexicon-kb*)

; Don't need to handle perfect aspect because it leads to the inference of past
; tense.  Progressive construction should be handled carefully since it will
; interact with aktionsart.
; John had run -> John ran
; John has run -> John ran
; John had built a house -> John built a house
; John has built a house -> John built a house

; The reference time of the embedded sentence is still the current time
; regardless of tense of the outer scope.
; 
; John confessed that he would hurt someone -> He would hurt someone
;                 -> He may or may not have hurt someone yet
; John confessed that he will hurt someone
;                 -> He did not hurt someone yet


; axioms: p = (X,Y) means p has implicativity X and Y in a positive and a negative 
; ^^^^^^      environment, respectively. X,Y are in {+,-,o}, each meaning positive, 
;             negative, and neutral. 
; accept = (+,o) 
; (see confess)
(all_wff w 
  (all_tense t
    (all x ((x ((t accept.v) (that w))) => w))))
(all_wff w 
  (all_tense t
    (all x ((x ((t prog) (accept.v (that w)))) => w))))

; acknowledge = (+,o) 
; (see confess)
(all_wff w 
  (all_tense t
    (all x ((x ((t acknowledge.v) (that w))) => w))))
(all_wff w 
  (all_tense t
    (all x ((x ((t prog) (acknowledge.v (that w)))) => w))))
; (see confess)
(all_pred p 
  (all_tense t
    (all x ((x ((t acknowledge.v) (ka p))) => (x p)))))
(all_pred p 
  (all_tense t
    (all x ((x ((t prog) (acknowledge.v (ka p)))) => (x p)))))

; admit = (+,o) 
; (see confess)
(all_wff w 
  (all_tense t
    (all x ((x ((t admit.v) (that w))) => w))))
(all_wff w 
  (all_tense t
    (all x ((x ((t prog) (admit.v (that w)))) => w))))
; (see confess)
(all_pred p 
  (all_tense t
    (all x ((x ((t admit.v) (ka p))) => (x p)))))
(all_pred p 
  (all_tense t
    (all x ((x ((t prog) (admit.v (ka p)))) => (x p)))))

; admonish = (+,+) [factive] 
; (see scold)
(((w_wff) (x) (y)) (x admonish.v y (that w)) w)
(((w_wff) (x) (y)) (not (x admonish.v y (that w))) w)

; agree = (+,o) 
; (see confess)
(all_wff w 
  (all_tense t
    (all x ((x ((t agree.v) (that w))) => (probably.adv-s w)))))
(all_wff w 
  (all_tense t
    (all x ((x ((t prog) (agree.v (that w)))) => (probably.adv-s w)))))
; (see confess)
(all_pred p 
  (all_tense t
    (all x ((x ((t agree.v) (ka p))) => (probably.adv-s (x p))))))
(all_pred p 
  (all_tense t
    (all x ((x ((t prog) (agree.v (ka p)))) => (probably.adv-s (x p))))))

; allow = (+,-) 
; John allowed Mary to have the prize. -> Mary could (i.e. was able to) have the prize.
; John was allowing Mary to have the prize. -> Mary could (i.e was able to) have the prize.
; John allows Mary to have the prize. -> Mary can have the prize.
; John is allowing Mary to have the prize. -> Mary can have the prize.
; GK(3/19/18): But what about other limitations of 'can'?  Like: "My mother
; allowed me to attend the concert but my father did not".  This implies that
; I cannot go to the concert.  Either we can mark this inference as
; presuppositional/implicated, or we can add a global mechanism more akin to
; traditional modal handling where the possibility must be entailed by the KB.
(all_pred p
  (all_tense t
    (all x 
      (all y ((x ((t allow.v) y (ka p))) 
           => (y ((t can.aux-v) p)))))))
(all_pred p
  (all_tense t
    (all x 
      (all y ((x ((t prog) (allow.v y (ka p)))) 
           => (y ((t can.aux-v) p)))))))
; John didn't allow Mary to have the prize    -> Mary could not have the prize
; John wasn't allowing Mary to have the prize -> Mary could not have the prize
; John doesn't allow Mary to have the prize   -> Mary can not have the prize
; John isn't allowing Mary to have the prize  -> Mary can not have the prize
; GK(3/19/18): We need some way to restrict the relative scoping of the
; negation and tense.  Say if the tense gets scoped outside of the negation:
;   (exists e [e before Now] ^ [neg S])
; vs
;   [neg (exists e [e before Now] ^ [S])]
; The first scoping is the eventwise *de re* reading, s.t. there is a specific
; past event in which the negation holds.  [This doesn't seem quite right, it
; seems rather than 'exists e', it should be 'the e', as in there is a relevant
; specific time?]. The second scoping is that there doesn't exist a time in the
; past where the S is true.  This doesn't quite seems right either because we
; can have "Mary won the race, but John didn't allow Mary to have the prize.
; Eventually, however, he changed his mind."  This example feels pretty
; constructed though... it turns out it's kind of hard to construct a natural
; example that really allows the full ambiguity shine.  Well regardless, either
; we need to make sure the scoping ends up being unimportant or unambiguous for
; the inference if we're not somehow tying the scoping to the antecedent scoping.
;
; This is because (exists e1 [e1 before Now] ^ [[neg [x allow.v y (ka p)]] ** e1]) 
;              => (exists e2 [e2 before Now] ^ [[neg [y (can.aux-v p)]] ** e2])
;            =/=> [neg (exists e2 [e2 before Now] ^ [[y (can.aux-v p)] ** e2])]
; Note however that the opposite works for both.
;  [neg (exists e1 [e1 before Now] ^ [[x allow.v y (ka p)] ** e1])]
;       => [neg (exists e2 [e2 before Now] ^ [[y (can.aux-v p)] ** e2])]
;  and  => (exists e2 [e2 before Now] ^ [[neg [y (can.aux-v p)]] ** e2]) [Well, if there is at least one event...]
(all_pred p
  (all_tense t
    (all x 
      (all y ((not (x ((t allow.v) y (ka p)))) 
           => (not (y ((t can.aux-v) p))))))))
(all_pred p
  (all_tense t
    (all x 
      (all y ((not (x ((t prog) (allow.v y (ka p))))) 
           => (not (y ((t can.aux-v) p))))))))

; amaze = (+,+) [factive] 
; (see shock)
(all_twff tw ; tensed wff
  (all_tense t
    (all x
      (((that tw) ((t amaze.v) x)) => tw))))
(all_twff tw ; tensed wff
  (all_tense t
    (all x
      ((not ((that tw) ((t amaze.v) x))) => tw))))
(all_twff tw ; tensed wff
  (all_tense t
    (all x
      (((that tw) ((t prog) (amaze.v x))) => tw))))
(all_twff tw ; tensed wff
  (all_tense t
    (all x
      ((not ((that tw) ((t prog) (amaze.v x)))) => tw))))

; amuse = (+,+) [factive] 
; (see shock)
(((tw_twff) (t_tense) (x)) ((that tw) ((t amuse.v) x)) tw)
(((tw_twff) (t_tense) (x)) (not ((that tw) ((t amuse.v) x))) tw)
(((tw_twff) (t_tense) (x)) ((that tw) ((t prog) (amuse.v x))) tw)
(((tw_twff) (t_tense) (x)) (not ((that tw) ((t prog) (amuse.v x)))) tw)

; annoy = (+,+) [factive] 
; (see shock)
(((tw_twff) (t_tense) (x)) ((that tw) ((t annoy.v) x)) tw)
(((tw_twff) (t_tense) (x)) (not ((that tw) ((t annoy.v) x))) tw)
(((tw_twff) (t_tense) (x)) ((that tw) ((t prog) (annoy.v x))) tw)
(((tw_twff) (t_tense) (x)) (not ((that tw) ((t prog) (annoy.v x)))) tw)

; appreciate = (+,+) [factive]
; (see neglect)
(((tw_twff) (t_tense) (x)) (x ((t appreciate.v) (that tw))) tw)
(((tw_twff) (t_tense) (x)) (not (x ((t appreciate.v) (that tw)))) tw)
(((tw_twff) (t_tense) (x)) (x ((t prog) (appreciate.v (that tw)))) tw)
(((tw_twff) (t_tense) (x)) (not (x ((t prog) (appreciate.v (that tw))))) tw)

; ascertain = (+,o)
; "The officer ascertain(s/ed) that John was sober." -> "John was sober."
; "The officer is/was ascertaining that John was sober." -> "John was sober." (not common)
(all_twff tw 
  (all_tense t
    (all x ((x ((t ascertain.v) (that tw))) => tw))))
(all_twff tw 
  (all_tense t
    (all x ((x ((t prog) (ascertain.v (that tw)))) => tw))))

; astonish = (+,+) [factive] 
; (see shock)
(((tw_twff) (t_tense) (x)) ((that tw) ((t astonish.v) x)) tw)
(((tw_twff) (t_tense) (x)) (not ((that tw) ((t astonish.v) x))) tw)
(((tw_twff) (t_tense) (x)) ((that tw) ((t prog) (astonish.v x))) tw)
(((tw_twff) (t_tense) (x)) (not ((that tw) ((t prog) (astonish.v x)))) tw)

; astound = (+,+) [factive] 
; (see shock)
(((tw_twff) (t_tense) (x)) ((that tw) ((t astound.v) x)) tw)
(((tw_twff) (t_tense) (x)) (not ((that tw) ((t astound.v) x))) tw)
(((tw_twff) (t_tense) (x)) ((that tw) ((t prog) (astound.v x))) tw)
(((tw_twff) (t_tense) (x)) (not ((that tw) ((t prog) (astound.v x)))) tw)

; attempt = (o,-)
; "John didn't attempt to see Mary." -> "John didn't see Mary."
; "John doesn't attempt to see Mary." -> "John doesn't see Mary."
; "John (is/was)n't attempting to see Mary." -> ? (seems to imply that he did...)
(all_pred p
  (all_tense t
    (all x ((not (x ((t attempt.v) (ka p)))) => (not (x (t p)))))))

; baffle = (+,+) [factive] 
; (see shock)
(((tw_twff) (t_tense) (x)) ((that tw) ((t baffle.v) x)) tw)
(((tw_twff) (t_tense) (x)) (not ((that tw) ((t baffle.v) x))) tw)
(((tw_twff) (t_tense) (x)) ((that tw) ((t prog) (baffle.v x))) tw)
(((tw_twff) (t_tense) (x)) (not ((that tw) ((t prog) (baffle.v x)))) tw)

; begin = (+,o)
; GK(3/18/18): The base form of the predicates are aktionsart dependent.
; "John began running" -> "John ran"
; "John began building a house" -/-> "John built a house"
; GK(3/18/18): however, the progressive form is not aktionsart dependent.
; "John began building a house" -> "John was building a house"
; "John begins to build a house" -> "John is building a house"
; "John began to learn piano." -> "John was learning piano" 
; All of these progressive inferences are for the time immediately following
; the reference time in the antecedent utterance.  The tense alone doesn't
; quite capture this.
(all_pred p
  (all_tense t
    (all x ((x ((t begin.v) (ka p))) =>
            (x ((t prog) p))))))

; berate = (+,+) [factive] 
; (see scold)
(((w_wff) (x) (y)) (x berate.v y (that w)) w)
(((w_wff) (x) (y)) (not (x berate.v y (that w))) w)

; bewilder = (+,+) [factive] 
; (see shock)
(((tw_twff) (t_tense) (x)) ((that tw) ((t bewilder.v) x)) tw)
(((tw_twff) (t_tense) (x)) (not ((that tw) ((t bewilder.v) x))) tw)
(((tw_twff) (t_tense) (x)) ((that tw) ((t prog) (bewilder.v x))) tw)
(((tw_twff) (t_tense) (x)) (not ((that tw) ((t prog) (bewilder.v x)))) tw)

; bother = (+,+) [factive] <that>
; (see shock)
(((tw_twff) (t_tense) (x)) ((that tw) ((t bother.v) x)) tw)
(((tw_twff) (t_tense) (x)) (not ((that tw) ((t bother.v) x))) tw)
(((tw_twff) (t_tense) (x)) ((that tw) ((t prog) (bother.v x))) tw)
(((tw_twff) (t_tense) (x)) (not ((that tw) ((t prog) (bother.v x)))) tw)

; bother = (+,-) <to>
; (see care)
(all_pred p
     (all x ((x bother.v (ka p)) => (x p)))))
; (see care)
(all_pred p
     (all x ((not (x bother.v (ka p))) => (not (x p))))))

; care = (+,+) [factive] <that>
; John care(s/d) that he got the award. -> he got the award
; John (does/did)n't care that he got the award. -> he got the award
; *John (is/was) caring that he got the award.
(((tw_twff) (t_tense) (x)) (x ((t care.v) (that tw))) tw)
(((tw_twff) (t_tense) (x)) (not (x ((t care.v) (that tw)))) tw)

; care = (+,-) <to>
; "John cared to read the report." -> "John read the report"
; "John cares to read the report." -> "John will read the report"/"John reads the report"
;   Depending on habitual or eventive reading.
;   "John cares to read the news." -> "John reads the news."
;   "John cares to read the news this morning." -> "John will read the news this morning."
(all_pred p
  (all x ((x ((past care.v) (ka p))) => (x (past p)))))
(all_pred p
  (all x ((x ((pres care.v) (ka p))) => (x ((pres will.aux-s) p)))))
; "John didn't care to grade it." -> "John didn't grade it."
; "John doesn't care to grade it." -> "John won't grade it."
(all_pred p
  (all x ((not (x ((past care.v) (ka p)))) => (not (x (past p))))))
(all_pred p
  (all x ((not (x ((pres care.v) (ka p)))) => (not (x ((pres will.aux-s) p))))))


; cease = ((+),+)
; (see stop)
(all_pred p
     (all x ((x cease.v (ka p)) => (past (x p)))))
; (see stop)
(all_pred p
     (all x ((not (x cease.v (ka p))) => (x p))))

; coerce = (+,o) 
; (see persuade)
(all_pred p
     (all x (all y ((x coerce.v (ka p) y) => (y p)))))

; come = (+,o)
; "John came to realize that the there are evil people."
(all_pred p
     (all x ((x come.v (ka p)) => (x p))))

; compel = (+,o) 
; (see persuade)
(all_pred p 
     (all x (all y ((x compel.v (ka p) y) => (y p)))))

; concede = (+,o) 
; John conceded that he lost. 
(((w_wff) (x)) (x concede.v (that w)) w)
; That John lost was conceded. 
(((w_wff)) ((that w) (pasv concede.v)) w)

; confess = (+,o) 
; "John confessed that he killed Tom." -> "He killed Tom."
; "John confesses that he killed Tom." -> "He killed Tom."
(all_wff w 
  (all_tense t
    (all x ((x ((t confess.v) (that w))) => w))))
; "John is confessing that he killed Tom." -> "He killed Tom."
; "John was confessing that he killed Tom." -> "He killed Tom."
(all_wff w 
  (all_tense t
    (all x ((x (((t prog) confess.v) (that w))) => w))))
; "John confessed to have eaten the cookies." -> "John has eaten the cookies"
; "John confesses to have eaten the cookies." -> "John has eaten the cookies"
; "John confesses/ed to having eaten the cookies." -> "John has eaten the cookies"
; "John confesses/ed to eating the cookies." -> "John ate the cookies."/"John has eaten the cookies."
(all_pred p 
  (all_tense t
    (all x ((x ((t confess.v) (ka p))) => (x p)))))
; "John was/is confessing to have eaten the cookies." -> "John ate the cookies."/"John has eaten the cookies."
(all_pred p 
  (all_tense t
    (all x ((x (((t prog) confess.v) (ka p))) => (x p)))))
; x will confess that w -> w

; Weak presuppositions
; x confess y 
; -> x regrets y; "John confessed that he committed the crime and was happy he did it" (surprising)
; -> x is the primary agent in y; "John confessed that Mark committed the crime" (not incorrect but weird)
; -> someone disapproves of y;


; confirm = (+,o) 
; (see praise)
(((w_wff) (x)) (x confirm.v (that w)) w)
; (see praise)
(((w_wff)) ((that w) (pasv confirm.v)) w)

; confuse = (+,+) [factive] 
; (see shock)
(((w_wff) (x)) ((that w) confuse.v x) w)
(((w_wff) (x)) (not ((that w) confuse.v x)) w)
; (see shock)
(((w_wff) (x)) (x (pasv confuse.v) (that w)) w)
(((w_wff) (x)) (not (x (pasv confuse.v) (that w))) w)

; continue = (+,(+))
; "John continues to live with Mary."
(all_pred p
     (all x ((x continue.v (ka p)) => (x p))))
; "John didn't continue to take piano lesson." 
(all_pred p
     (all x ((not (x continue.v (ka p))) => (past (x p)))))

; convince = (+,o) 
; (see persuade)
(all_pred p 
     (all x (all y ((x convince.v (ka p) y) => (y p)))))

; dare = (+,-)
; "John dared to challenge Tom." 
(all_pred p 
     (all x ((x dare.v (ka p)) => (x p))))
; "John didn't dare to challenge Mary."
(all_pred p 
     (all x ((not (x dare.v (ka p))) => (not (x p)))))

; decline = (-/(+)
; (see refuse)
(all_pred p
     (all x ((x decline.v (ka p)) => (not (x p)))))
; (see refuse)
(all_pred p
     (all x ((not (x decline.v (ka p))) => (probably (x p)))))

; delight = (+,+) [factive] 
; (see shock)
(((w_wff) (x)) ((that w) delight.v x) w)
(((w_wff) (x)) (not ((that w) delight.v x)) w)
; (see shock)
(((w_wff) (x)) (x (pasv delight.v) (that w)) w)
(((w_wff) (x)) (not (x (pasv delight.v) (that w))) w)

; demonstrate = (+,o) 
; (see praise)
(((w_wff) (x)) (x demonstrate.v (that w)) w)
; (see praise)
(((w_wff)) ((that w) (pasv demonstrate.v)) w)

; deplore = (+,+) [factive]
; (see praise)
(((w_wff) (x)) (x deplore.v (that w)) w)
(((w_wff) (x)) (not (x deplore.v (that w))) w)

; depress = (+,+) [factive] 
; (see shock)
(((w_wff) (x)) ((that w) depress.v x) w)
(((w_wff) (x)) (not ((that w) depress.v x)) w)
; (see shock)
(((w_wff) (x)) (x (pasv depress.v) (that w)) w)
(((w_wff) (x)) (not (x (pasv depress.v) (that w))) w)

; detest = (+,+) [factive]
; "John detests that his parents are alive."
(((w_wff) (x)) (x detest.v (that w)) w)
(((w_wff) (x)) (not (x detest.v (that w))) w)

; disappoint = (+,+) [factive] 
; (see shock)
(((w_wff) (x)) ((that w) disappoint.v x) w)
(((w_wff) (x)) (not ((that w) disappoint.v x)) w)
; (see shock)
(((w_wff) (x)) (x (pasv disappoint.v) (that w)) w)
(((w_wff) (x)) (not (x (pasv disappoint.v) (that w))) w)

; disconcert = (+,+) [factive] 
; (see shock)
(((w_wff) (x)) ((that w) disconcert.v x) w)
(((w_wff) (x)) (not ((that w) disconcert.v x)) w)
; (see shock)
(((w_wff) (x)) (x (pasv disconcert.v) (that w)) w)
(((w_wff) (x)) (not (x (pasv disconcert.v) (that w))) w)

; discourage = (+,+) [factive] 
; (see shock)
(((w_wff) (x)) ((that w) discourage.v x) w)
(((w_wff) (x)) (not ((that w) discourage.v x)) w)
; (see shock)
(((w_wff) (x)) (x (pasv discourage.v) (that w)) w)
(((w_wff) (x)) (not (x (pasv discourage.v) (that w))) w)

; discover = (+,o)
; (see find)
(all_wff w 
     (all x ((x discover.v (that w)) => w)))

; disgust = (+,+) [factive] 
; (see shock)
(((w_wff) (x)) ((that w) disgust.v x) w)
(((w_wff) (x)) (not ((that w) disgust.v x)) w)
; (see shock)
(((w_wff) (x)) (x (pasv disgust.v) (that w)) w)
(((w_wff) (x)) (not (x (pasv disgust.v) (that w))) w)

; disillusion = (+,+) [factive]
; (see shock)
(((w_wff) (x)) ((that w) disillusion.v x) w)
(((w_wff) (x)) (not ((that w) disillusion.v x)) w)
; (see shock)
(((w_wff) (x)) (x (pasv disillusion.v) (that w)) w)
(((w_wff) (x)) (not (x (pasv disillusion.v) (that w))) w)

; dislike = (+,+) [factive] <that>
; (see forget)
(((w_wff) (x)) (x dislike.v (that w)) w)
(((w_wff) (x)) (not (x dislike.v (that w))) w)

; disregard = (+,+) [factive]
; (see neglect)
(((w_wff) (x)) (x disregard.v (that w)) w)
(((w_wff) (x)) (not (x disregard.v (that w))) w)
; (see neglect)
(((w_wff)) ((that w) (pasv disregard.v)) w)
(((w_wff)) (not ((that w) (pasv disregard.v))) w)

; distress = (+,+) [factive] 
; (see shock)
(((w_wff) (x)) ((that w) distress.v x) w)
(((w_wff) (x)) (not ((that w) distress.v x)) w)
; (see shock)
(((w_wff) (x)) (x (pasv distress.v) (that w)) w)
(((w_wff) (x)) (not (x (pasv distress.v) (that w))) w)

; embarrass = (+,+) [factive] 
; (see shock)
(((w_wff) (x)) ((that w) embarrass.v x) w)
(((w_wff) (x)) (not ((that w) embarrass.v x)) w)
; (see shock)
(((w_wff) (x)) (x (pasv embarrass.v) (that w)) w)
(((w_wff) (x)) (not (x (pasv embarrass.v) (that w))) w)

; encourage = (+,+) [factive] 
; (see shock)
(((w_wff) (x)) ((that w) encourage.v x) w)
(((w_wff) (x)) (not ((that w) encourage.v x)) w)
; (see shock)
(((w_wff) (x)) (x (pasv encourage.v) (that w)) w)
(((w_wff) (x)) (not (x (pasv encourage.v) (that w))) w)

; envy = (+,+) [factive]
; (see praise)
(((w_wff) (x)) (x envy.v (that w)) w)
(((w_wff) (x)) (not (x envy.v (that w))) w)

; excite = (+,+) [factive] 
; (see shock)
(((w_wff) (x)) ((that w) excite.v x) w)
(((w_wff) (x)) (not ((that w) excite.v x)) w)
; (see shock)
(((w_wff) (x)) (x (pasv excite.v) (that w)) w)
(((w_wff) (x)) (not (x (pasv excite.v) (that w))) w)

; fail = (-,+)
; (see forget)
(all_pred p 
     (all x ((x fail.v (ka p)) => (not (x p)))))
; (see forget)
(all_pred p 
     (all x ((not (x fail.v (ka p))) => (x p))))

; find = (+,o)
; John found that Tom had slept with Mary. 
(all_wff w 
     (all x ((x find.v (that w)) => w)))
; John found Tom to be disgusting.
(all_pred p
     (all x (all y ((x find.v (ka p) y) => (probably (y p))))))


; force = (+,o)
; John forced Tom to clean the house.
(all_pred p 
     (all x (all y ((x force.v (ka p) y) => (y p)))))
(all_pred p
     (all x ((x (pasv force.v) (ka p)) => (x p))))

; forget = (+,+) [factive] <that>
; John forgot that the day was Tuesday.
(((w_wff) (x)) (x forget.v (that w)) w)
(((w_wff) (x)) (not (x forget.v (that w))) w)

; forget = (-,+) <to>
; "John forgot to bring the textbook." 
(all_pred p 
     (all x ((x forget.v (ka p)) => (not (x p)))))
; "John didn't forget to bring the homework." 
(all_pred p 
     (all x ((not (x forget.v (ka p))) => (x p))))

; get = (+,-)
; "John got to meet the president." 
(all_pred p 
     (all x ((x get.v (ka p)) => (x p))))
; "John didn't get to meet the vice president."
(all_pred p 
     (all x ((not (x get.v (ka p))) => (not (x p)))))

; happen = (+,o)
; John happened to witness the murder
(all_pred p 
     (all x ((x happen.v (ka p)) => (x p))))

; hate = (+,+)
; John hated to leave the place.
(((p_pred) (x)) (x hate.v (ka p)) (x p))
; John didn't hate to leave the place.
(((p_pred) (x)) (not (x hate.v (ka p))) (x p))

; hasten = (+,o)
; John hastened to go to the theater
(all_pred p 
     (all x ((x hasten.v (ka p)) => (x p))))

; help = (+,o)
; John helped Mary get a job
(all_pred p 
     (all x (all y ((x help.v (ka p) y) => (y p)))))

; hesitate = (o,(+)) 
; John didn't hesitate to accept the offer. 
(all_pred p 
     (all x ((not (x hesitate.v (ka p))) => (probably (x p)))))

; horrify = (+,+) [factive] 
; (see shock)
(((w_wff) (x)) ((that w) horrify.v x) w)
(((w_wff) (x)) (not ((that w) horrify.v x)) w)
; (see shock)
(((w_wff) (x)) (x (pasv horrify.v) (that w)) w)
(((w_wff) (x)) (not (x (pasv horrify.v) (that w))) w)

; ignore = (+,+) [factive]
; (see neglect)
(((w_wff) (x)) (x ignore.v (that w)) w)
(((w_wff) (x)) (not (x ignore.v (that w))) w)
; (see neglect)
(((w_wff)) ((that w) (pasv ignore.v)) w)
(((w_wff)) (not ((that w) (pasv ignore.v))) w)

; impress = (+,+) [factive] 
; (see shock)
(((w_wff) (x)) ((that w) impress.v x) w)
(((w_wff) (x)) (not ((that w) impress.v x)) w)
; (see shock)
(((w_wff) (x)) (x (pasv impress.v) (that w)) w)
(((w_wff) (x)) (not (x (pasv impress.v) (that w))) w)

; know = (+,+) [factive]
; (see praise)
(((w_wff) (x)) (x know.v (that w)) w)
(((w_wff) (x)) (not (x know.v (that w))) w)
; (see praise)
(((w_wff)) ((that w) (pasv know.v)) w)
(((w_wff)) (not ((that w) (pasv know.v))) w)

; lament = (+,+) [factive]
; (see praise)
(((w_wff) (x)) (x lament.v (that w)) w)
(((w_wff) (x)) (not (x lament.v (that w))) w)

; learn = (+,+) [factive]
; (see praise)
(((w_wff) (x)) (x learn.v (that w)) w)
(((w_wff) (x)) (not (x learn.v (that w))) w)

; lie = (-,-) [antifactive]
; "John lied that he was over 18."
(((w_wff) (x)) (x lie.v (that w)) (not w))
; "John didn't lie that he had a car." 
(((w_wff) (x)) (not (x lie.v (that w))) (not w))

; like = (+,o)
; (see love)
(all_pred p 
     (all x ((x like.v (ka p)) => (x p))))

; love = (+,o)
; "John loves to play tennis." 
(all_pred p 
     (all x ((x love.v (ka p)) => (x p))))

; manage = (+,-)
; "John managed to finish his history paper." 
(all_pred p 
     (all x ((x manage.v (ka p)) => (x p))))
; "John didn't manage to finish his computer project."
(all_pred p 
     (all x ((not (x manage.v (ka p))) => (not (x p)))))

; mean = (o,+) 
; "John didn't mean to harm Mary."
(all_pred p
     (all x ((past (not (x mean.v (ka p)))) => (x p))))

; mention = (o,+) <that>
; "John didn't mention that the plant was dry." 
(((w_wff) (x)) (not (x mention.v (that w))) w)
; "That the dog was hungry wasn't mentioned." 
(((w_wff)) (not ((that w) (pasv mention.v))) w)

; miss = (+,+) [factive]
; "John misses having a dog." 
(((p_pred) (x)) (x miss.v (ka p)) (past (x p)))
; "John doesn't miss having a cat." 
(((p_pred) (x)) (not (x miss.v (ka p))) (past (x p)))

; neglect = (+,+) [factive] <that>
; TODO: take a look at Aaron White's paper which seems relevant:
;       http://aswhite.net/papers/white_factive-implicatives_2014.pdf
; "John neglect(s/ed) that the plant (was/is) dry."
; "John (did/does)n't neglect that the plant (was/is) dry."
; "John is/was[n't] neglecting that his sister need(s/ed) his help."
(((tw_twff) (t_tense) (x)) (x ((t neglect.v) (that tw))) tw)
(((tw_twff) (t_tense) (x)) (not (x ((t neglect.v) (that tw)))) tw)
(((tw_twff) (t_tense) (x)) (x ((t prog) (neglect.v (that tw)))) tw)
(((tw_twff) (t_tense) (x)) (not (x ((t prog) (neglect.v (that tw))))) tw)
; TODO: Also infer that tw is being neglected? -- That's probably beyond the
; scope of our current work. 

; neglect = (-,+) <to>
; GK(3/20/18): is there some way we can capture that the inferred statements
; are habitual rather than a single event for the present tense and
; progressives?  I guess the habituality is tied to whether the antecedent is
; habitual.  Maybe what we need a linking of habituality between the antecedent
; and consequent.
; "John neglected to water the plant." -> "John didn't water the plant."
; "John neglects to water the plant." -> "John doesn't water the plant."
; "John was neglecting to water the plant." -> "John wasn't watering his plant."
; "John is neglecting to water the plant." -> "John isn't watering his plant."
(all_pred p
  (all_tense t
    (all x ((x ((t neglect.v) (ka p))) => (not (x (t p))))))) ; TODO: the tense can't be added like that...
(all_pred p
  (all_tense t
    (all x ((x ((t prog) (neglect.v (ka p)))) => (not (x ((t prog) p))))))) 
; "John didn't neglect to feed his dog." -> "John fed his dog."
; "John doesn't neglect to feed his dog." -> "John feeds his dog."
; "John wasn't neglecting to feed his dog." -> "John was feeding his dog."
; "John isn't neglecting to feed his dog." -> "John is feeding his dog."
(all_pred p
  (all_tense t
    (all x ((not (x ((t neglect.v) (ka p)))) => (x (t p))))))
(all_pred p
  (all_tense t
    (all x ((not (x ((t prog) (neglect.v (ka p))))) => (x ((t prog) p))))))

; note = (+,+) [factive]
; (see notice)
(((w_wff) (x)) (x note.v (that w)) w)
(((w_wff) (x)) (not (x note.v (that w))) w)

; notice = (+,+) [factive]
; "John didn't notice that Mary was gone." 
(((w_wff) (x)) (x notice.v (that w)) w)
(((w_wff) (x)) (not (x notice.v (that w))) w)

; observe = (+,o) 
; "John observed that chemistry could be very useful." 
(all_wff w
     (all x ((x observe.v (that w)) => w)))

; outrage = (+,+) [factive] 
; "That Mary cheated him outrages John." 
(((w_wff) (x)) ((that w) outrage.v x) w)
(((w_wff) (x)) (not ((that w) outrage.v x)) w)
; "John isn't outraged that Tom dated Mary." 
(((w_wff) (x)) (x (pasv outrage.v) (that w)) w)
(((w_wff) (x)) (not (x (pasv outrage.v) (that w))) w)

; overlook = (+,+) [factive]
; (see praise)
(((w_wff) (x)) (x overlook.v (that w)) w)
(((w_wff) (x)) (not (x overlook.v (that w))) w)
; (see praise)
(((w_wff)) ((that w) (pasv overlook.v)) w)
(((w_wff)) (not ((that w) (pasv overlook.v))) w)

; permit = (+,-) 
; (see allow)
(all_pred p
     (all x (all y ((x permit.v (ka p) y) => (y (can.v p))))))
; (see allow)
(all_pred p
     (all x (all y ((not (x permit.v (ka p) y)) => (not (y (can.v p)))))))

; persuade = (+,o) 
; "John persuaded Tom to write a book." 
(all_pred p 
     (all x (all y ((x persuade.v (ka p) y) => (y p)))))

; pity = (+,+) [factive]
; (see praise)
(((w_wff) (x)) (x pity.v (that w)) w)
(((w_wff) (x)) (not (x pity.v (that w))) w)
; (see praise)
(((w_wff)) ((that w) (pasv pity.v)) w)
(((w_wff)) (not ((that w) (pasv pity.v))) w)

; please = (+,+) [factive]
; "John is pleased that he got a raise."
(((w_wff) (x)) (x (pasv please.v) (that w)) w)
(((w_wff) (x)) (not (x (pasv please.v) (that w))) w)
; "That he lost his bonus didn't please John." 
(((w_wff) (x)) ((that w) please.v x) w)
(((w_wff) (x)) (not ((that w) please.v x)) w)

; praise = (+,+) [factive]
; "John praised that the kitchen was clean."
(((w_wff) (x)) (x praise.v (that w)) w)
(((w_wff) (x)) (not (x praise.v (that w))) w)
; "That John cheated wasn't praised." 
(((w_wff)) ((that w) (pasv praise.v)) w)
(((w_wff)) (not ((that w) (pasv praise.v))) w)

; pretend = (-,-) [antifactive]
; "John pretended that he was a doctor."
(((w_wff) (x)) (x pretend.v (that w)) (not w))
(((w_wff) (x)) (not (x pretend.v (that w))) (probably (not w)))
; "John didn't pretend to know math." 
(((p_pred) (x)) (x pretend.v (ka p)) (not (x p)))
(((p_pred) (x)) (not (x pretend.v (ka p))) (probably (not (x p))))

; prohibit = (-,o)
; "His parents prohibited John to drink rum." 
(all_pred p  
     (all x (all y ((x prohibit.v (ka p) y) => (not (y p))))))

; prove = (+,o)
; "John proved that he was an officer." 
(all_wff w 
     (all x ((x prove.v (that w)) => w)))
; "That John was an officer was proved."
(all_wff w (((that w) (pasv prove.v)) => w))
; "John proved to be a formidable opponent."
(all x (all y ((x prove.v y) => (x is-a y))))

; realize = (+,+) [factive]
; "John realized that the signature was fake." 
(((w_wff) (x)) (x realize.v (that w)) w)
(((w_wff) (x)) (not (x realize.v (that w))) w)

; recall = (+,o) <that> 
; (see recollect)
(all_wff w 
     (all x ((x recall.v (that w)) => w)))

; recollect = (+,o) <that> 
; "John recollects that his childhood was brutal." 
(all_wff w 
     (all x ((x recollect.v (that w)) => w)))

; recognize = (+,+) [factive]
; (see realize)
(((w_wff) (x)) (x recognize.v (that w)) w)
(((w_wff) (x)) (not (x recognize.v (that w))) w)

; reflect = (+,o) <that> 
; "John reflected that he used to succeed in running a marathon." 
(all_wff w 
     (all x ((x reflect.v (that w)) => w)))

; refrain-from = (-,+) 
; "John refrains from drinking vodka." 
(all_pred p
     (all x ((x refrain-from.v (ka p)) => (not (x p)))))
; "John doesn't refrain from drinking beer."
(all_pred p
     (all x ((not (x refrain-from.v (ka p))) => (x p))))

; refuse = (-/(+)
; "John refused to drink ginger tea." 
(all_pred p
     (all x ((x refuse.v (ka p)) => (not (x p)))))
; "John didn't refuse to drink coffee." 
(all_pred p
     (all x ((not (x refuse.v (ka p))) => (probably (x p)))))

; regret = (+,+) [factive]
; "John regretted that he treated Mary harshly."
(((w_wff) (x)) (x regret.v (that w)) w)
(((w_wff) (x)) (not (x regret.v (that w))) w)
; "John doesn't regret having treated Tom harshly." 
(((p_pred) (x)) (x regret.v (ka p)) (x p))
(((p_pred) (x)) (not (x regret.v (ka p))) (x p))
; "That the money got lost wasn't regreted."
(((w_wff)) ((that w) (pasv regret.v)) w)
(((w_wff)) (not ((that w) (pasv regret.v))) w)
; x regretted that w -> w
; x has/had regretted that w -> w
; x regrets running home -> x ran home
; x regretted running home -> x had run home
; x regrets having run home -> x has run home
; x regretted having run home -> x had run home
; x has (not) regretted y-ing -> x has y-ed
; x had (not) regretted y-ing -> x had y-ed
; x will (not) regret y-ing -> x [plans to/will probably] y



; rejoice = (+,+) [factive]
; "John didn't rejoice that the war ended."
(((w_wff) (x)) (x rejoice.v (that w)) w)
(((w_wff) (x)) (not (x rejoice.v (that w))) w)

; remember = (+,o) <that> 
; "John remembered that he had no money." 
(all_wff w 
     (all x ((x remember.v (that w)) => w)))

; remember = (+,-) <to> 
; "John remembered to bring a pencil." 
(all_pred p
     (all x ((x remember.v (ka p)) => (x p))))
; "John didn't remember to bring an eraser." 
(all_pred p
     (all x ((not (x remember.v (ka p))) => (not (x p)))))

; remind = (+,o)
; "John reminded Mary to lock the door." 
(all_pred p 
     (all x (all y ((x remind.v (ka p) y) => (probably (y p))))))

; restrain-from = (-,+)
; "John restrained from drinking." 
(all_pred p
     (all x ((x restrain-from.v (ka p)) => (not (x p)))))
; "John didn't restrain from smoking."
(all_pred p
     (all x ((not (x restrain-from.v (ka p))) => (x p))))

; resume = (+,(+)
; (see continue)
(all_pred p
     (all x ((x resume.v (ka p)) => (x p))))
; (see continue)
(all_pred p
     (all x ((not (x resume.v (ka p))) => (past (x p)))))

; reveal = (+,+) [factive]
; "John revealed that he hated Tom."
(((w_wff) (x)) (x reveal.v (that w)) w)
(((w_wff) (x)) (not (x reveal.v (that w))) w)
; "That John loved Mary wasn't revealed."
(((w_wff)) ((that w) (pasv reveal.v)) w)
(((w_wff)) (not ((that w) (pasv reveal.v))) w)

; scare = (+,+) [factive] 
; "That he has cancer scares John." 
(((w_wff) (x)) ((that w) scare.v x) w)
(((w_wff) (x)) (not ((that w) scare.v x)) w)
; "John isn't scared that he has a cold." 
(((w_wff) (x)) (x (pasv scare.v) (that w)) w)
(((w_wff) (x)) (not (x (pasv scare.v) (that w))) w)

; GK(3/18/18) - the complementizer construction seems strange to me, the
; versions I added underneath seem more common.  Also the presupposed/implied
; info doesn't seem as strong as other factives.  More akin to "say".  "John
; said Mary made a mess" vs "John scolded Mary that she made a mess".  "John
; didn't say that Mary made a mess" vs "John didn't scold Mary that she made a
; mess".  Compare to "John knew that Mary made a mess" and "John didn't know
; that Mary a made a mess." NOTE: these comments hold for all the entries that
; point to this with "(see scold)".
; I did a COCA search for 'berate', another word with similar uses and saw no
; examples of "x berate y that ..." of this meaning in the 196 examples that
; came up.
; scold = (+,+) [factive] 
; "John scolded Mary that she left a mess."
; "John didn't scold Mary that she left a mess."
(((w_wff) (x) (y)) (x scold.v y (that w)) w)
(((w_wff) (x) (y)) (not (x scold.v y (that w))) w)
; Non-complementizer version.
; TODO: write rules for this...
; "John scolded Mary for/about the mess she left."
; "John scolded Mary for leaving a mess"
; "John is scolding Mary for/about the mess she left."
; "John scolded Mary for/about leaving a mess"
; "John scolded Mary for/about having left a mess"
; John scolded Mary as a slob - John 
; John scolded Mary into following his plan


; see = (+,o)  
; "we see that D and N commute with T." 
(((w_wff) (x)) (x see.v (that w)) w))

; shock = (+,+) [factive]
; GK(3/19/18): Like other proper factives the embedded sentence is tensed in
; relation to the outer utterance time, so the tense of the implied fact is
; unchanged (see below).
; "That his friend betrayed him shock(s/ed) John." -> "His friend betrayed him"
; "That his friend would betray him shocked John." -> "His friend would betray him."
; "That his friend will betray him shock(s/ed) John." 
;   -> "His friend will betray him." (hasn't happened yet in utterance time)
;     Hmmm... this one sounds a bit funny, is the past tense utterance for this acceptable?
; "That his friend would betray him shocks John." -- Not possible with (past
; will) interpretation because the embedding is a past-future, but the outer
; statement is in present tense.  However, still possible with would.aux-s to
; assert possibility.

; Long versions.
;(all_twff tw ; tensed wff
;  (all_tense t
;    (all x
;      (((that tw) ((t shock.v) x)) => tw))))
;(all_twff tw ; tensed wff
;  (all_tense t
;    (all x
;      ((not ((that tw) ((t shock.v) x))) => tw))))
;(all_twff tw ; tensed wff
;  (all_tense t
;    (all x
;      (((that tw) ((t prog) (shock.v x))) => tw))))
;(all_twff tw ; tensed wff
;  (all_tense t
;    (all x
;      ((not ((that tw) ((t prog) (shock.v x)))) => tw))))
(((tw_twff) (t_tense) (x)) ((that tw) ((t shock.v) x)) tw)
(((tw_twff) (t_tense) (x)) (not ((that tw) ((t shock.v) x))) tw)
(((tw_twff) (t_tense) (x)) ((that tw) ((t prog) (shock.v x))) tw)
(((tw_twff) (t_tense) (x)) (not ((that tw) ((t prog) (shock.v x)))) tw)
; "John is(n't) shocked that his friend betrayed him." -> "His friend betrayed him"
; GK(3/19/18): Not including passive construction because that inference
; generalization holds true for all the verbs.  The passive construction
; inferences should be generative from the base rules.

; show = (+,o) 
; "Einstein showed that time is not absolute." 
(all_wff w 
     (all x ((x show.v (that w)) => w))))

; start = (+,o)
; "John started to cry."
(all_pred p
     (all x ((x start.v (ka p)) => (x p)))))

; stop = ((+),+)
; "John stopped reading newspaper." 
(all_pred p 
     (all x ((x stop.v (ka p)) => (past (x p))))))
; "John didn't stop reading newspaper." 
(all_pred p 
     (all x ((not (x stop.v (ka p))) => (x p)))))

; succeed-in = (+,-) 
; "John succeeded in climbing Himalayas."
(all_pred p 
     (all x ((x succeed-in.v (ka p)) => (x p)))))
; "John didn't succeed in escaping from his wife."
(all_pred p
     (all x ((not (x succeed-in.v (ka p))) => (not (x p))))))

; surprise = (+,+) [factive] 
; "That fish don't blink surprises John." 
(((w_wff) (x)) ((that w) surprise.v x) w))
(((w_wff) (x)) (not ((that w) surprise.v x)) w))
; "John isn't surprised that the mind is mechanical." 
(((w_wff) (x)) (x (pasv surprise.v) (that w)) w))
(((w_wff) (x)) (not (x (pasv surprise.v) (that w))) w))

; suspect = (o,(+))
; "John didn't suspect that Mary was innocent."
(all_wff w
     (all x ((not (x suspect.v (that w))) =>  (probably w)))))

; tend = (+,o) 
; "John tended to break his glasses." 
(all_pred p 
     (all x ((x tend.v (ka p)) => (x p)))))

; touch = (+,+) [factive]
; "John is touched that the children love their father."
(((w_wff) (x)) (x (pasv touch.v) (that w)) w))
(((w_wff) (x)) (not (x (pasv touch.v) (that w))) w))
; "That the children love their father doesn't touch John." 
(((w_wff) (x)) ((that w) touch.v x) w))
(((w_wff) (x)) (not ((that w) touch.v x)) w))

; trouble = (+,+) [factive]
; "John is troubled that Mary is sick."
(((w_wff) (x)) (x (pasv trouble.v) (that w)) w))
(((w_wff) (x)) (not (x (pasv trouble.v) (that w))) w))
; "That Mary is sick doens't trouble John."
(((w_wff) (x)) ((that w) trouble.v x) w))
(((w_wff) (x)) (not ((that w) trouble.v x)) w))

; turn-out = (+,-) 
; "The program turned out to be a fraud"
(all x (all y ((x turn-out.v y) => (x is-a y)))))
; "John didn't turn out to be rich (after all)" 
(all x (all y ((not (x turn-out.v y)) => (not (x is-a y))))))

; understand = (+,+) [factive]
; "John understands that the situation is grim."
(((w_wff) (x)) (x understand.v (that w)) w))
(((w_wff) (x)) (not (x understand.v (that w))) w))
; "That the situation is grim isn't understood."
(((w_wff)) ((that w) (pasv understand.v)) w))
(((w_wff)) (not ((that w) (pasv understand.v))) w))

; unnerve = (+,+) [factive] 
; "That the war broke out unnerves John." 
(((w_wff) (x)) ((that w) unnerve.v x) w))
(((w_wff) (x)) (not ((that w) unnerve.v x)) w))
; "John isn't unnerved that his wallet is gone." 
(((w_wff) (x)) (x (pasv unnerve.v) (that w)) w))
(((w_wff) (x)) (not (x (pasv unnerve.v) (that w))) w))

; use = (+,o)
; "John used Lisp to write EPILOG."
(all_pred p
     (all x (all y ((x use.v y (ka p)) => (x p))))))

; used-to = (+,o)
; "John used to work for the government." 
(all_pred p 
     (all x ((x used-to.v (ka p)) => (past (x p))))))

; verify = (+,o)
; "John verified that the mushroom is poisonous." 
(all_wff w
     (all x ((x verify.v (that w)) => w))))
; "That the mushroom is poisonous was verified"
(all_wff w (((that w) (pasv verify.v)) => w)))

; warn = (o,+) 
; "John didn't warn that the job was hard!"
(all_wff w
     (all x ((not (x warn.v (that w))) => w))))
; "That the job was hard wasn't warned." 
(all_wff w ((not ((that w) (pasv warn.v))) => w)))
