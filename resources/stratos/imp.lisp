;-------------------------------------------------------------
; List of implicatives and their lexical semantic axioms
;   -Karl Stratos (7/11) 
;-------------------------------------------------------------
(in-package epi)

; for queries that take over 10 seconds, use my-pq and my-dq 
(defun my-pq (q1)
  (pq (p q1) :stop-fn-on-answers #'has-been-answered?))
(defun my-dq (q1)
  (dq (p q1) :stop-fn-on-answers #'has-been-answered?))

; declaration: store unknown predicates in *lexicon-kb*. 
; ^^^^^^^^^^^
(store (x-is-predicate 'accept.v) *lexicon-kb*)
(store (x-is-predicate 'acknowledge.v) *lexicon-kb*)
(store (x-is-predicate 'admit.v) *lexicon-kb*)
(store (x-is-predicate 'admonish.v) *lexicon-kb*)
(store (x-is-predicate 'agree.v) *lexicon-kb*)
(store (x-is-predicate 'allow.v) *lexicon-kb*)
(store (x-is-predicate 'amaze.v) *lexicon-kb*)
(store (x-is-predicate 'amuse.v) *lexicon-kb*)
(store (x-is-predicate 'annoy.v) *lexicon-kb*)
(store (x-is-predicate 'appreciate.v) *lexicon-kb*)
(store (x-is-predicate 'ascertain.v) *lexicon-kb*)
(store (x-is-predicate 'astonish.v) *lexicon-kb*)
(store (x-is-predicate 'astound.v) *lexicon-kb*)
(store (x-is-predicate 'attempt.v) *lexicon-kb*)
(store (x-is-predicate 'baffle.v) *lexicon-kb*)
(store (x-is-predicate 'begin.v) *lexicon-kb*)
(store (x-is-predicate 'berate.v) *lexicon-kb*)
(store (x-is-predicate 'bewilder.v) *lexicon-kb*)
(store (x-is-predicate 'bother.v) *lexicon-kb*)
(store (x-is-predicate 'care.v) *lexicon-kb*)
(store (x-is-predicate 'cease.v) *lexicon-kb*)
(store (x-is-predicate 'coerce.v) *lexicon-kb*)
(store (x-is-predicate 'come.v) *lexicon-kb*)
(store (x-is-predicate 'compel.v) *lexicon-kb*)
(store (x-is-predicate 'concede.v) *lexicon-kb*)
(store (x-is-predicate 'confess.v) *lexicon-kb*)
(store (x-is-predicate 'confirm.v) *lexicon-kb*)
(store (x-is-predicate 'confuse.v) *lexicon-kb*)
(store (x-is-predicate 'continue.v) *lexicon-kb*)
(store (x-is-predicate 'convince.v) *lexicon-kb*)
(store (x-is-predicate 'dare.v) *lexicon-kb*)
(store (x-is-predicate 'decline.v) *lexicon-kb*)
(store (x-is-predicate 'delight.v) *lexicon-kb*)
(store (x-is-predicate 'demonstrate.v) *lexicon-kb*)
(store (x-is-predicate 'deplore.v) *lexicon-kb*)
(store (x-is-predicate 'depress.v) *lexicon-kb*)
(store (x-is-predicate 'detest.v) *lexicon-kb*)
(store (x-is-predicate 'disappoint.v) *lexicon-kb*)
(store (x-is-predicate 'disconcert.v) *lexicon-kb*)
(store (x-is-predicate 'discourage.v) *lexicon-kb*)
(store (x-is-predicate 'discover.v) *lexicon-kb*)
(store (x-is-predicate 'disgust.v) *lexicon-kb*)
(store (x-is-predicate 'disillusion.v) *lexicon-kb*)
(store (x-is-predicate 'dislike.v) *lexicon-kb*)
(store (x-is-predicate 'disregard.v) *lexicon-kb*)
(store (x-is-predicate 'distress.v) *lexicon-kb*)
(store (x-is-predicate 'embarrass.v) *lexicon-kb*)
(store (x-is-predicate 'encourage.v) *lexicon-kb*)
(store (x-is-predicate 'envy.v) *lexicon-kb*)
(store (x-is-predicate 'excite.v) *lexicon-kb*)
(store (x-is-predicate 'fail.v) *lexicon-kb*)
(store (x-is-predicate 'find.v) *lexicon-kb*)
(store (x-is-predicate 'force.v) *lexicon-kb*)
(store (x-is-predicate 'forget.v) *lexicon-kb*)
(store (x-is-predicate 'get.v) *lexicon-kb*)
(store (x-is-predicate 'happen.v) *lexicon-kb*)
(store (x-is-predicate 'hasten.v) *lexicon-kb*)
(store (x-is-predicate 'help.v) *lexicon-kb*)
(store (x-is-predicate 'hesitate.v) *lexicon-kb*)
(store (x-is-predicate 'horrify.v) *lexicon-kb*)
(store (x-is-predicate 'ignore.v) *lexicon-kb*)
(store (x-is-predicate 'impress.v) *lexicon-kb*)
(store (x-is-predicate 'lament.v) *lexicon-kb*)
(store (x-is-predicate 'learn.v) *lexicon-kb*)
(store (x-is-predicate 'lie.v) *lexicon-kb*)
(store (x-is-predicate 'like.v) *lexicon-kb*)
(store (x-is-predicate 'manage.v) *lexicon-kb*)
(store (x-is-predicate 'mention.v) *lexicon-kb*)
(store (x-is-predicate 'miss.v) *lexicon-kb*)
(store (x-is-predicate 'neglect.v) *lexicon-kb*)
(store (x-is-predicate 'note.v) *lexicon-kb*)
(store (x-is-predicate 'notice.v) *lexicon-kb*)
(store (x-is-predicate 'observe.v) *lexicon-kb*)
(store (x-is-predicate 'outrage.v) *lexicon-kb*)
(store (x-is-predicate 'overlook.v) *lexicon-kb*)
(store (x-is-predicate 'permit.v) *lexicon-kb*)
(store (x-is-predicate 'persuade.v) *lexicon-kb*)
(store (x-is-predicate 'pity.v) *lexicon-kb*)
(store (x-is-predicate 'please.v) *lexicon-kb*)
(store (x-is-predicate 'praise.v) *lexicon-kb*)
(store (x-is-predicate 'pretend.v) *lexicon-kb*)
(store (x-is-predicate 'prohibit.v) *lexicon-kb*)
(store (x-is-predicate 'prove.v) *lexicon-kb*)
(store (x-is-predicate 'realize.v) *lexicon-kb*)
(store (x-is-predicate 'recall.v) *lexicon-kb*)
(store (x-is-predicate 'recollect.v) *lexicon-kb*)
(store (x-is-predicate 'recognize.v) *lexicon-kb*)
(store (x-is-predicate 'reflect.v) *lexicon-kb*)
(store (x-is-predicate 'refrain-from.v) *lexicon-kb*)
(store (x-is-predicate 'refuse.v) *lexicon-kb*)
(store (x-is-predicate 'regret.v) *lexicon-kb*)
(store (x-is-predicate 'rejoice.v) *lexicon-kb*)
(store (x-is-predicate 'remember.v) *lexicon-kb*)
(store (x-is-predicate 'remind.v) *lexicon-kb*)
(store (x-is-predicate 'restrain-from.v) *lexicon-kb*)
(store (x-is-predicate 'resume.v) *lexicon-kb*)
(store (x-is-predicate 'reveal.v) *lexicon-kb*)
(store (x-is-predicate 'see.v) *lexicon-kb*)
(store (x-is-predicate 'scare.v) *lexicon-kb*)
(store (x-is-predicate 'scold.v) *lexicon-kb*)
(store (x-is-predicate 'shock.v) *lexicon-kb*)
(store (x-is-predicate 'show.v) *lexicon-kb*)
(store (x-is-predicate 'start.v) *lexicon-kb*)
(store (x-is-predicate 'stop.v) *lexicon-kb*)
(store (x-is-predicate 'succeed-in.v) *lexicon-kb*)
(store (x-is-predicate 'surprise.v) *lexicon-kb*)
(store (x-is-predicate 'suspect.v) *lexicon-kb*)
(store (x-is-predicate 'tend.v) *lexicon-kb*)
(store (x-is-predicate 'touch.v) *lexicon-kb*)
(store (x-is-predicate 'trouble.v) *lexicon-kb*)
(store (x-is-predicate 'turn.v) *lexicon-kb*)
(store (x-is-predicate 'understand.v) *lexicon-kb*)
(store (x-is-predicate 'unnerve.v) *lexicon-kb*)
(store (x-is-predicate 'use.v) *lexicon-kb*)
(store (x-is-predicate 'used-to.v) *lexicon-kb*)
(store (x-is-predicate 'verify.v) *lexicon-kb*)
(store (x-is-predicate 'warn.v) *lexicon-kb*)
(store (x-is-pred-mod 'can.v) *lexicon-kb*)

; axioms: p = (X,Y) means p has implicativity X and Y in a positive and a negative 
; ^^^^^^      environment, respectively. X,Y are in {+,-,o}, each meaning positive, 
;             negative, and neutral. 
; accept = (+,o) 
; (see confess)
(s '(all_wff w
     (all x ((x accept.v (that w)) => w))))

; acknowledge = (+,o) 
; (see confess)
(s '(all_wff w 
     (all x ((x acknowledge.v (that w)) => w))))
; (see confess)
(s '(all_pred p 
     (all x ((x acknowledge.v (ka p)) => (x p)))))

; admit = (+,o) 
; (see confess)
(s '(all_wff w 
     (all x ((x admit.v (that w)) => w))))
; (see confess)
(s '(all_pred p 
     (all x ((x admit.v (ka p)) => (x p)))))

; admonish = (+,+) [factive] 
; (see scold)
(store-prs-ir '(((w_wff) (x) (y)) (x admonish.v y (that w)) w))
(store-prs-ir '(((w_wff) (x) (y)) (not (x admonish.v y (that w))) w))

; agree = (+,o) 
; (see confess)
(s '(all_wff w 
     (all x ((x agree.v (that w)) => (probably w)))))
; (see confess)
(s '(all_pred p 
     (all x ((x agree.v (ka p)) => (probably (x p))))))

; allow = (+,-) 
; John allowed Mary to have the prize.
(s '(all_pred p
     (all x (all y ((x allow.v (ka p) y) => (y (can.v p)))))))
; John didn't allow Mary to have the prize
(s '(all_pred p
     (all x (all y ((not (x allow.v (ka p) y)) => (not (y (can.v p))))))))

; amaze = (+,+) [factive] 
; (see shock)
(store-prs-ir '(((w_wff) (x)) ((that w) amaze.v x) w))
(store-prs-ir '(((w_wff) (x)) (not ((that w) amaze.v x)) w))
; (see shock)
(store-prs-ir '(((w_wff) (x)) (x (pasv amaze.v) (that w)) w))
(store-prs-ir '(((w_wff) (x)) (not (x (pasv amaze.v) (that w))) w))

; amuse = (+,+) [factive] 
; (see shock)
(store-prs-ir '(((w_wff) (x)) ((that w) amuse.v x) w))
(store-prs-ir '(((w_wff) (x)) (not ((that w) amuse.v x)) w))
; (see shock)
(store-prs-ir '(((w_wff) (x)) (x (pasv amuse.v) (that w)) w))
(store-prs-ir '(((w_wff) (x)) (not (x (pasv amuse.v) (that w))) w))

; annoy = (+,+) [factive] 
; (see shock)
(store-prs-ir '(((w_wff) (x)) ((that w) annoy.v x) w))
(store-prs-ir '(((w_wff) (x)) (not ((that w) annoy.v x)) w))
; (see shock)
(store-prs-ir '(((w_wff) (x)) (x (pasv annoy.v) (that w)) w))
(store-prs-ir '(((w_wff) (x)) (not (x (pasv annoy.v) (that w))) w))

; appreciate = (+,+) [factive]
; (see neglect)
(store-prs-ir '(((w_wff) (x)) (x appreciate.v (that w)) w))
(store-prs-ir '(((w_wff) (x)) (not (x appreciate.v (that w))) w))
; (see neglect)
(store-prs-ir '(((w_wff)) ((that w) (pasv appreciate.v)) w))
(store-prs-ir '(((w_wff)) (not ((that w) (pasv appreciate.v))) w))

; ascertain = (+,o)
; "The officer ascertained that John was sober." 
(s '(all_wff w 
     (all x ((x ascertain.v (that w)) => w))))
; "That John was not drunk was ascertained."
(s '(all_wff w (((that w) (pasv ascertain.v)) => w)))

; astonish = (+,+) [factive] 
; (see shock)
(store-prs-ir '(((w_wff) (x)) ((that w) astonish.v x) w))
(store-prs-ir '(((w_wff) (x)) (not ((that w) astonish.v x)) w))
; (see shock)
(store-prs-ir '(((w_wff) (x)) (x (pasv astonish.v) (that w)) w))
(store-prs-ir '(((w_wff) (x)) (not (x (pasv astonish.v) (that w))) w))

; astound = (+,+) [factive] 
; (see shock)
(store-prs-ir '(((w_wff) (x)) ((that w) astound.v x) w))
(store-prs-ir '(((w_wff) (x)) (not ((that w) astound.v x)) w))
; (see shock)
(store-prs-ir '(((w_wff) (x)) (x (pasv astound.v) (that w)) w))
(store-prs-ir '(((w_wff) (x)) (not (x (pasv astound.v) (that w))) w))

; attempt = (o,-)
; "John didn't attempt to see Mary." 
(s '(all_pred p
     (all x ((not (x attempt.v (ka p))) => (not (x p))))))

; baffle = (+,+) [factive] 
; (see shock)
(store-prs-ir '(((w_wff) (x)) ((that w) baffle.v x) w))
(store-prs-ir '(((w_wff) (x)) (not ((that w) baffle.v x)) w))
; (see shock)
(store-prs-ir '(((w_wff) (x)) (x (pasv baffle.v) (that w)) w))
(store-prs-ir '(((w_wff) (x)) (not (x (pasv baffle.v) (that w))) w))

; begin = (+,o)
; "John began to learn piano." 
(s '(all_pred p
     (all x ((x begin.v (ka p)) => (x p)))))

; berate = (+,+) [factive] 
; (see scold)
(store-prs-ir '(((w_wff) (x) (y)) (x berate.v y (that w)) w))
(store-prs-ir '(((w_wff) (x) (y)) (not (x berate.v y (that w))) w))

; bewilder = (+,+) [factive] 
; (see shock)
(store-prs-ir '(((w_wff) (x)) ((that w) bewilder.v x) w))
(store-prs-ir '(((w_wff) (x)) (not ((that w) bewilder.v x)) w))
; (see shock)
(store-prs-ir '(((w_wff) (x)) (x (pasv bewilder.v) (that w)) w))
(store-prs-ir '(((w_wff) (x)) (not (x (pasv bewilder.v) (that w))) w))

; bother = (+,+) [factive] <that>
; (see shock)
(store-prs-ir '(((w_wff) (x)) ((that w) bother.v x) w))
(store-prs-ir '(((w_wff) (x)) (not ((that w) bother.v x)) w))
; (see shock)
(store-prs-ir '(((w_wff) (x)) (x (pasv bother.v) (that w)) w))
(store-prs-ir '(((w_wff) (x)) (not (x (pasv bother.v) (that w))) w))

; bother = (+,-) <to>
; (see care)
(s '(all_pred p
     (all x ((x bother.v (ka p)) => (x p)))))
; (see care)
(s '(all_pred p
     (all x ((not (x bother.v (ka p))) => (not (x p))))))

; care = (+,+) [factive] <that>
; John didn't care that he got the award.
(store-prs-ir '(((w_wff) (x)) (x care.v (that w)) w))
(store-prs-ir '(((w_wff) (x)) (not (x care.v (that w))) w))

; care = (+,-) <to>
; "John cared to read the report."
(s '(all_pred p
     (all x ((x care.v (ka p)) => (x p)))))
; "John didn't care to grade it."
(s '(all_pred p
     (all x ((not (x care.v (ka p))) => (not (x p))))))

; cease = ((+),+)
; (see stop)
(s '(all_pred p
     (all x ((x cease.v (ka p)) => (past (x p))))))
; (see stop)
(s '(all_pred p
     (all x ((not (x cease.v (ka p))) => (x p)))))

; coerce = (+,o) 
; (see persuade)
(s '(all_pred p
     (all x (all y ((x coerce.v (ka p) y) => (y p))))))

; come = (+,o)
; "John came to realize that the there are evil people."
(s '(all_pred p
     (all x ((x come.v (ka p)) => (x p)))))

; compel = (+,o) 
; (see persuade)
(s '(all_pred p 
     (all x (all y ((x compel.v (ka p) y) => (y p))))))

; concede = (+,o) 
; John conceded that he lost. 
(store-prs-ir '(((w_wff) (x)) (x concede.v (that w)) w))
; That John lost was conceded. 
(store-prs-ir '(((w_wff)) ((that w) (pasv concede.v)) w))

; confess = (+,o) 
; "John confessed that he killed Tom."
(s '(all_wff w 
     (all x ((x confess.v (that w)) => w))))
; "John confessed to have eaten the cookies." 
(s '(all_pred p 
     (all x ((x confess.v (ka p)) => (x p)))))

; confirm = (+,o) 
; (see praise)
(store-prs-ir '(((w_wff) (x)) (x confirm.v (that w)) w))
; (see praise)
(store-prs-ir '(((w_wff)) ((that w) (pasv confirm.v)) w))

; confuse = (+,+) [factive] 
; (see shock)
(store-prs-ir '(((w_wff) (x)) ((that w) confuse.v x) w))
(store-prs-ir '(((w_wff) (x)) (not ((that w) confuse.v x)) w))
; (see shock)
(store-prs-ir '(((w_wff) (x)) (x (pasv confuse.v) (that w)) w))
(store-prs-ir '(((w_wff) (x)) (not (x (pasv confuse.v) (that w))) w))

; continue = (+,(+))
; "John continues to live with Mary."
(s '(all_pred p
     (all x ((x continue.v (ka p)) => (x p)))))
; "John didn't continue to take piano lesson." 
(s '(all_pred p
     (all x ((not (x continue.v (ka p))) => (past (x p))))))

; convince = (+,o) 
; (see persuade)
(s '(all_pred p 
     (all x (all y ((x convince.v (ka p) y) => (y p))))))

; dare = (+,-)
; "John dared to challenge Tom." 
(s '(all_pred p 
     (all x ((x dare.v (ka p)) => (x p)))))
; "John didn't dare to challenge Mary."
(s '(all_pred p 
     (all x ((not (x dare.v (ka p))) => (not (x p))))))

; decline = (-/(+))
; (see refuse)
(s '(all_pred p
     (all x ((x decline.v (ka p)) => (not (x p))))))
; (see refuse)
(s '(all_pred p
     (all x ((not (x decline.v (ka p))) => (probably (x p))))))

; delight = (+,+) [factive] 
; (see shock)
(store-prs-ir '(((w_wff) (x)) ((that w) delight.v x) w))
(store-prs-ir '(((w_wff) (x)) (not ((that w) delight.v x)) w))
; (see shock)
(store-prs-ir '(((w_wff) (x)) (x (pasv delight.v) (that w)) w))
(store-prs-ir '(((w_wff) (x)) (not (x (pasv delight.v) (that w))) w))

; demonstrate = (+,o) 
; (see praise)
(store-prs-ir '(((w_wff) (x)) (x demonstrate.v (that w)) w))
; (see praise)
(store-prs-ir '(((w_wff)) ((that w) (pasv demonstrate.v)) w))

; deplore = (+,+) [factive]
; (see praise)
(store-prs-ir '(((w_wff) (x)) (x deplore.v (that w)) w))
(store-prs-ir '(((w_wff) (x)) (not (x deplore.v (that w))) w))

; depress = (+,+) [factive] 
; (see shock)
(store-prs-ir '(((w_wff) (x)) ((that w) depress.v x) w))
(store-prs-ir '(((w_wff) (x)) (not ((that w) depress.v x)) w))
; (see shock)
(store-prs-ir '(((w_wff) (x)) (x (pasv depress.v) (that w)) w))
(store-prs-ir '(((w_wff) (x)) (not (x (pasv depress.v) (that w))) w))

; detest = (+,+) [factive]
; "John detests that his parents are alive."
(store-prs-ir '(((w_wff) (x)) (x detest.v (that w)) w))
(store-prs-ir '(((w_wff) (x)) (not (x detest.v (that w))) w))

; disappoint = (+,+) [factive] 
; (see shock)
(store-prs-ir '(((w_wff) (x)) ((that w) disappoint.v x) w))
(store-prs-ir '(((w_wff) (x)) (not ((that w) disappoint.v x)) w))
; (see shock)
(store-prs-ir '(((w_wff) (x)) (x (pasv disappoint.v) (that w)) w))
(store-prs-ir '(((w_wff) (x)) (not (x (pasv disappoint.v) (that w))) w))

; disconcert = (+,+) [factive] 
; (see shock)
(store-prs-ir '(((w_wff) (x)) ((that w) disconcert.v x) w))
(store-prs-ir '(((w_wff) (x)) (not ((that w) disconcert.v x)) w))
; (see shock)
(store-prs-ir '(((w_wff) (x)) (x (pasv disconcert.v) (that w)) w))
(store-prs-ir '(((w_wff) (x)) (not (x (pasv disconcert.v) (that w))) w))

; discourage = (+,+) [factive] 
; (see shock)
(store-prs-ir '(((w_wff) (x)) ((that w) discourage.v x) w))
(store-prs-ir '(((w_wff) (x)) (not ((that w) discourage.v x)) w))
; (see shock)
(store-prs-ir '(((w_wff) (x)) (x (pasv discourage.v) (that w)) w))
(store-prs-ir '(((w_wff) (x)) (not (x (pasv discourage.v) (that w))) w))

; discover = (+,o)
; (see find)
(s '(all_wff w 
     (all x ((x discover.v (that w)) => w))))

; disgust = (+,+) [factive] 
; (see shock)
(store-prs-ir '(((w_wff) (x)) ((that w) disgust.v x) w))
(store-prs-ir '(((w_wff) (x)) (not ((that w) disgust.v x)) w))
; (see shock)
(store-prs-ir '(((w_wff) (x)) (x (pasv disgust.v) (that w)) w))
(store-prs-ir '(((w_wff) (x)) (not (x (pasv disgust.v) (that w))) w))

; disillusion = (+,+) [factive]
; (see shock)
(store-prs-ir '(((w_wff) (x)) ((that w) disillusion.v x) w))
(store-prs-ir '(((w_wff) (x)) (not ((that w) disillusion.v x)) w))
; (see shock)
(store-prs-ir '(((w_wff) (x)) (x (pasv disillusion.v) (that w)) w))
(store-prs-ir '(((w_wff) (x)) (not (x (pasv disillusion.v) (that w))) w))

; dislike = (+,+) [factive] <that>
; (see forget)
(store-prs-ir '(((w_wff) (x)) (x dislike.v (that w)) w))
(store-prs-ir '(((w_wff) (x)) (not (x dislike.v (that w))) w))

; disregard = (+,+) [factive]
; (see neglect)
(store-prs-ir '(((w_wff) (x)) (x disregard.v (that w)) w))
(store-prs-ir '(((w_wff) (x)) (not (x disregard.v (that w))) w))
; (see neglect)
(store-prs-ir '(((w_wff)) ((that w) (pasv disregard.v)) w))
(store-prs-ir '(((w_wff)) (not ((that w) (pasv disregard.v))) w))

; distress = (+,+) [factive] 
; (see shock)
(store-prs-ir '(((w_wff) (x)) ((that w) distress.v x) w))
(store-prs-ir '(((w_wff) (x)) (not ((that w) distress.v x)) w))
; (see shock)
(store-prs-ir '(((w_wff) (x)) (x (pasv distress.v) (that w)) w))
(store-prs-ir '(((w_wff) (x)) (not (x (pasv distress.v) (that w))) w))

; embarrass = (+,+) [factive] 
; (see shock)
(store-prs-ir '(((w_wff) (x)) ((that w) embarrass.v x) w))
(store-prs-ir '(((w_wff) (x)) (not ((that w) embarrass.v x)) w))
; (see shock)
(store-prs-ir '(((w_wff) (x)) (x (pasv embarrass.v) (that w)) w))
(store-prs-ir '(((w_wff) (x)) (not (x (pasv embarrass.v) (that w))) w))

; encourage = (+,+) [factive] 
; (see shock)
(store-prs-ir '(((w_wff) (x)) ((that w) encourage.v x) w))
(store-prs-ir '(((w_wff) (x)) (not ((that w) encourage.v x)) w))
; (see shock)
(store-prs-ir '(((w_wff) (x)) (x (pasv encourage.v) (that w)) w))
(store-prs-ir '(((w_wff) (x)) (not (x (pasv encourage.v) (that w))) w))

; envy = (+,+) [factive]
; (see praise)
(store-prs-ir '(((w_wff) (x)) (x envy.v (that w)) w))
(store-prs-ir '(((w_wff) (x)) (not (x envy.v (that w))) w))

; excite = (+,+) [factive] 
; (see shock)
(store-prs-ir '(((w_wff) (x)) ((that w) excite.v x) w))
(store-prs-ir '(((w_wff) (x)) (not ((that w) excite.v x)) w))
; (see shock)
(store-prs-ir '(((w_wff) (x)) (x (pasv excite.v) (that w)) w))
(store-prs-ir '(((w_wff) (x)) (not (x (pasv excite.v) (that w))) w))

; fail = (-,+)
; (see forget)
(s '(all_pred p 
     (all x ((x fail.v (ka p)) => (not (x p))))))
; (see forget)
(s '(all_pred p 
     (all x ((not (x fail.v (ka p))) => (x p)))))

; find = (+,o)
; John found that Tom had slept with Mary. 
(s '(all_wff w 
     (all x ((x find.v (that w)) => w))))
; John found Tom to be disgusting.
(s '(all_pred p
     (all x (all y ((x find.v (ka p) y) => (probably (y p)))))))


; force = (+,o)
; John forced Tom to clean the house.
(s '(all_pred p 
     (all x (all y ((x force.v (ka p) y) => (y p))))))
(s '(all_pred p
     (all x ((x (pasv force.v) (ka p)) => (x p)))))

; forget = (+,+) [factive] <that>
; John forgot that the day was Tuesday.
(store-prs-ir '(((w_wff) (x)) (x forget.v (that w)) w))
(store-prs-ir '(((w_wff) (x)) (not (x forget.v (that w))) w))

; forget = (-,+) <to>
; "John forgot to bring the textbook." 
(s '(all_pred p 
     (all x ((x forget.v (ka p)) => (not (x p))))))
; "John didn't forget to bring the homework." 
(s '(all_pred p 
     (all x ((not (x forget.v (ka p))) => (x p)))))

; get = (+,-)
; "John got to meet the president." 
(s '(all_pred p 
     (all x ((x get.v (ka p)) => (x p)))))
; "John didn't get to meet the vice president."
(s '(all_pred p 
     (all x ((not (x get.v (ka p))) => (not (x p))))))

; happen = (+,o)
; John happened to witness the murder
(s '(all_pred p 
     (all x ((x happen.v (ka p)) => (x p)))))

; hate = (+,+)
; John hated to leave the place.
(store-prs-ir '(((p_pred) (x)) (x hate.v (ka p)) (x p)))
; John didn't hate to leave the place.
(store-prs-ir '(((p_pred) (x)) (not (x hate.v (ka p))) (x p)))

; hasten = (+,o)
; John hastened to go to the theater
(s '(all_pred p 
     (all x ((x hasten.v (ka p)) => (x p)))))

; help = (+,o)
; John helped Mary get a job
(s '(all_pred p 
     (all x (all y ((x help.v (ka p) y) => (y p))))))

; hesitate = (o,(+)) 
; John didn't hesitate to accept the offer. 
(s '(all_pred p 
     (all x ((not (x hesitate.v (ka p))) => (probably (x p))))))

; horrify = (+,+) [factive] 
; (see shock)
(store-prs-ir '(((w_wff) (x)) ((that w) horrify.v x) w))
(store-prs-ir '(((w_wff) (x)) (not ((that w) horrify.v x)) w))
; (see shock)
(store-prs-ir '(((w_wff) (x)) (x (pasv horrify.v) (that w)) w))
(store-prs-ir '(((w_wff) (x)) (not (x (pasv horrify.v) (that w))) w))

; ignore = (+,+) [factive]
; (see neglect)
(store-prs-ir '(((w_wff) (x)) (x ignore.v (that w)) w))
(store-prs-ir '(((w_wff) (x)) (not (x ignore.v (that w))) w))
; (see neglect)
(store-prs-ir '(((w_wff)) ((that w) (pasv ignore.v)) w))
(store-prs-ir '(((w_wff)) (not ((that w) (pasv ignore.v))) w))

; impress = (+,+) [factive] 
; (see shock)
(store-prs-ir '(((w_wff) (x)) ((that w) impress.v x) w))
(store-prs-ir '(((w_wff) (x)) (not ((that w) impress.v x)) w))
; (see shock)
(store-prs-ir '(((w_wff) (x)) (x (pasv impress.v) (that w)) w))
(store-prs-ir '(((w_wff) (x)) (not (x (pasv impress.v) (that w))) w))

; know = (+,+) [factive]
; (see praise)
(store-prs-ir '(((w_wff) (x)) (x know.v (that w)) w))
(store-prs-ir '(((w_wff) (x)) (not (x know.v (that w))) w))
; (see praise)
(store-prs-ir '(((w_wff)) ((that w) (pasv know.v)) w))
(store-prs-ir '(((w_wff)) (not ((that w) (pasv know.v))) w))

; lament = (+,+) [factive]
; (see praise)
(store-prs-ir '(((w_wff) (x)) (x lament.v (that w)) w))
(store-prs-ir '(((w_wff) (x)) (not (x lament.v (that w))) w))

; learn = (+,+) [factive]
; (see praise)
(store-prs-ir '(((w_wff) (x)) (x learn.v (that w)) w))
(store-prs-ir '(((w_wff) (x)) (not (x learn.v (that w))) w))

; lie = (-,-) [antifactive]
; "John lied that he was over 18."
(store-prs-ir '(((w_wff) (x)) (x lie.v (that w)) (not w)))
; "John didn't lie that he had a car." 
(store-prs-ir '(((w_wff) (x)) (not (x lie.v (that w))) (not w)))

; like = (+,o)
; (see love)
(s '(all_pred p 
     (all x ((x like.v (ka p)) => (x p)))))

; love = (+,o)
; "John loves to play tennis." 
(s '(all_pred p 
     (all x ((x love.v (ka p)) => (x p)))))

; manage = (+,-)
; "John managed to finish his history paper." 
(s '(all_pred p 
     (all x ((x manage.v (ka p)) => (x p)))))
; "John didn't manage to finish his computer project."
(s '(all_pred p 
     (all x ((not (x manage.v (ka p))) => (not (x p))))))

; mean = (o,+) 
; "John didn't mean to harm Mary."
(s '(all_pred p
     (all x ((past (not (x mean.v (ka p)))) => (x p)))))

; mention = (o,+) <that>
; "John didn't mention that the plant was dry." 
(store-prs-ir '(((w_wff) (x)) (not (x mention.v (that w))) w))
; "That the dog was hungry wasn't mentioned." 
(store-prs-ir '(((w_wff)) (not ((that w) (pasv mention.v))) w))

; miss = (+,+) [factive]
; "John misses having a dog." 
(store-prs-ir '(((p_pred) (x)) (x miss.v (ka p)) (past (x p))))
; "John doesn't miss having a cat." 
(store-prs-ir '(((p_pred) (x)) (not (x miss.v (ka p))) (past (x p))))

; neglect = (+,+) [factive] <that>
; "John neglected that the plant was dry." 
(store-prs-ir '(((w_wff) (x)) (x neglect.v (that w)) w))
(store-prs-ir '(((w_wff) (x)) (not (x neglect.v (that w))) w))
; "That the dog was hungry wasn't neglected." 
(store-prs-ir '(((w_wff)) ((that w) (pasv neglect.v)) w))
(store-prs-ir '(((w_wff)) (not ((that w) (pasv neglect.v))) w))

; neglect = (-,+) <to>
; "John neglected to water the plant." 
(s '(all_pred p
     (all x ((x neglect.v (ka p)) => (not (x p))))))
; "John didn't neglect to feed his dog." 
(s '(all_pred p
     (all x ((not (x neglect.v (ka p))) => (x p)))))

; note = (+,+) [factive]
; (see notice)
(store-prs-ir '(((w_wff) (x)) (x note.v (that w)) w))
(store-prs-ir '(((w_wff) (x)) (not (x note.v (that w))) w))

; notice = (+,+) [factive]
; "John didn't notice that Mary was gone." 
(store-prs-ir '(((w_wff) (x)) (x notice.v (that w)) w))
(store-prs-ir '(((w_wff) (x)) (not (x notice.v (that w))) w))

; observe = (+,o) 
; "John observed that chemistry could be very useful." 
(s '(all_wff w
     (all x ((x observe.v (that w)) => w))))

; outrage = (+,+) [factive] 
; "That Mary cheated him outrages John." 
(store-prs-ir '(((w_wff) (x)) ((that w) outrage.v x) w))
(store-prs-ir '(((w_wff) (x)) (not ((that w) outrage.v x)) w))
; "John isn't outraged that Tom dated Mary." 
(store-prs-ir '(((w_wff) (x)) (x (pasv outrage.v) (that w)) w))
(store-prs-ir '(((w_wff) (x)) (not (x (pasv outrage.v) (that w))) w))

; overlook = (+,+) [factive]
; (see praise)
(store-prs-ir '(((w_wff) (x)) (x overlook.v (that w)) w))
(store-prs-ir '(((w_wff) (x)) (not (x overlook.v (that w))) w))
; (see praise)
(store-prs-ir '(((w_wff)) ((that w) (pasv overlook.v)) w))
(store-prs-ir '(((w_wff)) (not ((that w) (pasv overlook.v))) w))

; permit = (+,-) 
; (see allow)
(s '(all_pred p
     (all x (all y ((x permit.v (ka p) y) => (y (can.v p)))))))
; (see allow)
(s '(all_pred p
     (all x (all y ((not (x permit.v (ka p) y)) => (not (y (can.v p))))))))

; persuade = (+,o) 
; "John persuaded Tom to write a book." 
(s '(all_pred p 
     (all x (all y ((x persuade.v (ka p) y) => (y p))))))

; pity = (+,+) [factive]
; (see praise)
(store-prs-ir '(((w_wff) (x)) (x pity.v (that w)) w))
(store-prs-ir '(((w_wff) (x)) (not (x pity.v (that w))) w))
; (see praise)
(store-prs-ir '(((w_wff)) ((that w) (pasv pity.v)) w))
(store-prs-ir '(((w_wff)) (not ((that w) (pasv pity.v))) w))

; please = (+,+) [factive]
; "John is pleased that he got a raise."
(store-prs-ir '(((w_wff) (x)) (x (pasv please.v) (that w)) w))
(store-prs-ir '(((w_wff) (x)) (not (x (pasv please.v) (that w))) w))
; "That he lost his bonus didn't please John." 
(store-prs-ir '(((w_wff) (x)) ((that w) please.v x) w))
(store-prs-ir '(((w_wff) (x)) (not ((that w) please.v x)) w))

; praise = (+,+) [factive]
; "John praised that the kitchen was clean."
(store-prs-ir '(((w_wff) (x)) (x praise.v (that w)) w))
(store-prs-ir '(((w_wff) (x)) (not (x praise.v (that w))) w))
; "That John cheated wasn't praised." 
(store-prs-ir '(((w_wff)) ((that w) (pasv praise.v)) w))
(store-prs-ir '(((w_wff)) (not ((that w) (pasv praise.v))) w))

; pretend = (-,-) [antifactive]
; "John pretended that he was a doctor."
(store-prs-ir '(((w_wff) (x)) (x pretend.v (that w)) (not w)))
(store-prs-ir '(((w_wff) (x)) (not (x pretend.v (that w))) (probably (not w))))
; "John didn't pretend to know math." 
(store-prs-ir '(((p_pred) (x)) (x pretend.v (ka p)) (not (x p))))
(store-prs-ir '(((p_pred) (x)) (not (x pretend.v (ka p))) (probably (not (x p)))))

; prohibit = (-,o)
; "His parents prohibited John to drink rum." 
(s '(all_pred p  
     (all x (all y ((x prohibit.v (ka p) y) => (not (y p)))))))

; prove = (+,o)
; "John proved that he was an officer." 
(s '(all_wff w 
     (all x ((x prove.v (that w)) => w))))
; "That John was an officer was proved."
(s '(all_wff w (((that w) (pasv prove.v)) => w)))
; "John proved to be a formidable opponent."
(s '(all x (all y ((x prove.v y) => (x is-a y)))))

; realize = (+,+) [factive]
; "John realized that the signature was fake." 
(store-prs-ir '(((w_wff) (x)) (x realize.v (that w)) w))
(store-prs-ir '(((w_wff) (x)) (not (x realize.v (that w))) w))

; recall = (+,o) <that> 
; (see recollect)
(s '(all_wff w 
     (all x ((x recall.v (that w)) => w)))) 

; recollect = (+,o) <that> 
; "John recollects that his childhood was brutal." 
(s '(all_wff w 
     (all x ((x recollect.v (that w)) => w)))) 

; recognize = (+,+) [factive]
; (see realize)
(store-prs-ir '(((w_wff) (x)) (x recognize.v (that w)) w))
(store-prs-ir '(((w_wff) (x)) (not (x recognize.v (that w))) w))

; reflect = (+,o) <that> 
; "John reflected that he used to succeed in running a marathon." 
(s '(all_wff w 
     (all x ((x reflect.v (that w)) => w)))) 

; refrain-from = (-,+) 
; "John refrains from drinking vodka." 
(s '(all_pred p
     (all x ((x refrain-from.v (ka p)) => (not (x p))))))
; "John doesn't refrain from drinking beer."
(s '(all_pred p
     (all x ((not (x refrain-from.v (ka p))) => (x p)))))

; refuse = (-/(+))
; "John refused to drink ginger tea." 
(s '(all_pred p
     (all x ((x refuse.v (ka p)) => (not (x p))))))
; "John didn't refuse to drink coffee." 
(s '(all_pred p
     (all x ((not (x refuse.v (ka p))) => (probably (x p))))))

; regret = (+,+) [factive]
; "John regretted that he treated Mary harshly."
(store-prs-ir '(((w_wff) (x)) (x regret.v (that w)) w))
(store-prs-ir '(((w_wff) (x)) (not (x regret.v (that w))) w))
; "John doesn't regret having treated Tom harshly." 
(store-prs-ir '(((p_pred) (x)) (x regret.v (ka p)) (x p)))
(store-prs-ir '(((p_pred) (x)) (not (x regret.v (ka p))) (x p)))
; "That the money got lost wasn't regreted."
(store-prs-ir '(((w_wff)) ((that w) (pasv regret.v)) w))
(store-prs-ir '(((w_wff)) (not ((that w) (pasv regret.v))) w))

; rejoice = (+,+) [factive]
; "John didn't rejoice that the war ended."
(store-prs-ir '(((w_wff) (x)) (x rejoice.v (that w)) w))
(store-prs-ir '(((w_wff) (x)) (not (x rejoice.v (that w))) w))

; remember = (+,o) <that> 
; "John remembered that he had no money." 
(s '(all_wff w 
     (all x ((x remember.v (that w)) => w))))

; remember = (+,-) <to> 
; "John remembered to bring a pencil." 
(s '(all_pred p
     (all x ((x remember.v (ka p)) => (x p)))))
; "John didn't remember to bring an eraser." 
(s '(all_pred p
     (all x ((not (x remember.v (ka p))) => (not (x p))))))

; remind = (+,o)
; "John reminded Mary to lock the door." 
(s '(all_pred p 
     (all x (all y ((x remind.v (ka p) y) => (probably (y p)))))))

; restrain-from = (-,+)
; "John restrained from drinking." 
(s '(all_pred p
     (all x ((x restrain-from.v (ka p)) => (not (x p))))))
; "John didn't restrain from smoking."
(s '(all_pred p
     (all x ((not (x restrain-from.v (ka p))) => (x p)))))

; resume = (+,(+))
; (see continue)
(s '(all_pred p
     (all x ((x resume.v (ka p)) => (x p)))))
; (see continue)
(s '(all_pred p
     (all x ((not (x resume.v (ka p))) => (past (x p))))))

; reveal = (+,+) [factive]
; "John revealed that he hated Tom."
(store-prs-ir '(((w_wff) (x)) (x reveal.v (that w)) w))
(store-prs-ir '(((w_wff) (x)) (not (x reveal.v (that w))) w))
; "That John loved Mary wasn't revealed."
(store-prs-ir '(((w_wff)) ((that w) (pasv reveal.v)) w))
(store-prs-ir '(((w_wff)) (not ((that w) (pasv reveal.v))) w))

; scare = (+,+) [factive] 
; "That he has cancer scares John." 
(store-prs-ir '(((w_wff) (x)) ((that w) scare.v x) w))
(store-prs-ir '(((w_wff) (x)) (not ((that w) scare.v x)) w))
; "John isn't scared that he has a cold." 
(store-prs-ir '(((w_wff) (x)) (x (pasv scare.v) (that w)) w))
(store-prs-ir '(((w_wff) (x)) (not (x (pasv scare.v) (that w))) w))

; scold = (+,+) [factive] 
; "John scolded Mary that she left a mess." 
(store-prs-ir '(((w_wff) (x) (y)) (x scold.v y (that w)) w))
(store-prs-ir '(((w_wff) (x) (y)) (not (x scold.v y (that w))) w))

; see = (+,o)  
; "we see that D and N commute with T." 
(store-prs-ir '(((w_wff) (x)) (x see.v (that w)) w))

; shock = (+,+) [factive] 
; "That his friend betrayed him shocks John." 
(store-prs-ir '(((w_wff) (x)) ((that w) shock.v x) w))
(store-prs-ir '(((w_wff) (x)) (not ((that w) shock.v x)) w))
; "John isn't shocked that his friend betrayed him." 
(store-prs-ir '(((w_wff) (x)) (x (pasv shock.v) (that w)) w))
(store-prs-ir '(((w_wff) (x)) (not (x (pasv shock.v) (that w))) w))

; show = (+,o) 
; "Einstein showed that time is not absolute." 
(s '(all_wff w 
     (all x ((x show.v (that w)) => w))))

; start = (+,o)
; "John started to cry."
(s '(all_pred p
     (all x ((x start.v (ka p)) => (x p)))))

; stop = ((+),+)
; "John stopped reading newspaper." 
(s '(all_pred p 
     (all x ((x stop.v (ka p)) => (past (x p))))))
; "John didn't stop reading newspaper." 
(s '(all_pred p 
     (all x ((not (x stop.v (ka p))) => (x p)))))

; succeed-in = (+,-) 
; "John succeeded in climbing Himalayas."
(s '(all_pred p 
     (all x ((x succeed-in.v (ka p)) => (x p)))))
; "John didn't succeed in escaping from his wife."
(s '(all_pred p
     (all x ((not (x succeed-in.v (ka p))) => (not (x p))))))

; surprise = (+,+) [factive] 
; "That fish don't blink surprises John." 
(store-prs-ir '(((w_wff) (x)) ((that w) surprise.v x) w))
(store-prs-ir '(((w_wff) (x)) (not ((that w) surprise.v x)) w))
; "John isn't surprised that the mind is mechanical." 
(store-prs-ir '(((w_wff) (x)) (x (pasv surprise.v) (that w)) w))
(store-prs-ir '(((w_wff) (x)) (not (x (pasv surprise.v) (that w))) w))

; suspect = (o,(+))
; "John didn't suspect that Mary was innocent."
(s '(all_wff w
     (all x ((not (x suspect.v (that w))) =>  (probably w)))))

; tend = (+,o) 
; "John tended to break his glasses." 
(s '(all_pred p 
     (all x ((x tend.v (ka p)) => (x p)))))

; touch = (+,+) [factive]
; "John is touched that the children love their father."
(store-prs-ir '(((w_wff) (x)) (x (pasv touch.v) (that w)) w))
(store-prs-ir '(((w_wff) (x)) (not (x (pasv touch.v) (that w))) w))
; "That the children love their father doesn't touch John." 
(store-prs-ir '(((w_wff) (x)) ((that w) touch.v x) w))
(store-prs-ir '(((w_wff) (x)) (not ((that w) touch.v x)) w))

; trouble = (+,+) [factive]
; "John is troubled that Mary is sick."
(store-prs-ir '(((w_wff) (x)) (x (pasv trouble.v) (that w)) w))
(store-prs-ir '(((w_wff) (x)) (not (x (pasv trouble.v) (that w))) w))
; "That Mary is sick doens't trouble John."
(store-prs-ir '(((w_wff) (x)) ((that w) trouble.v x) w))
(store-prs-ir '(((w_wff) (x)) (not ((that w) trouble.v x)) w))

; turn-out = (+,-) 
; "The program turned out to be a fraud"
(s '(all x (all y ((x turn-out.v y) => (x is-a y)))))
; "John didn't turn out to be rich (after all)" 
(s '(all x (all y ((not (x turn-out.v y)) => (not (x is-a y))))))

; understand = (+,+) [factive]
; "John understands that the situation is grim."
(store-prs-ir '(((w_wff) (x)) (x understand.v (that w)) w))
(store-prs-ir '(((w_wff) (x)) (not (x understand.v (that w))) w))
; "That the situation is grim isn't understood."
(store-prs-ir '(((w_wff)) ((that w) (pasv understand.v)) w))
(store-prs-ir '(((w_wff)) (not ((that w) (pasv understand.v))) w))

; unnerve = (+,+) [factive] 
; "That the war broke out unnerves John." 
(store-prs-ir '(((w_wff) (x)) ((that w) unnerve.v x) w))
(store-prs-ir '(((w_wff) (x)) (not ((that w) unnerve.v x)) w))
; "John isn't unnerved that his wallet is gone." 
(store-prs-ir '(((w_wff) (x)) (x (pasv unnerve.v) (that w)) w))
(store-prs-ir '(((w_wff) (x)) (not (x (pasv unnerve.v) (that w))) w))

; use = (+,o)
; "John used Lisp to write EPILOG."
(s '(all_pred p
     (all x (all y ((x use.v y (ka p)) => (x p))))))

; used-to = (+,o)
; "John used to work for the government." 
(s '(all_pred p
     (all x ((x used-to.v (ka p)) => (past (x p))))))

; verify = (+,o)
; "John verified that the mushroom is poisonous." 
(s '(all_wff w
     (all x ((x verify.v (that w)) => w))))
; "That the mushroom is poisonous was verified"
(s '(all_wff w (((that w) (pasv verify.v)) => w)))

; warn = (o,+) 
; "John didn't warn that the job was hard!"
(s '(all_wff w
     (all x ((not (x warn.v (that w))) => w))))
; "That the job was hard wasn't warned." 
(s '(all_wff w ((not ((that w) (pasv warn.v))) => w)))
