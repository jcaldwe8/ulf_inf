;; Nov 4/17
;; SEE ./README FOR THE PURPOSE OF THIS CODE.
;; THIS FILE DEPENDS ON TTT.
;; =========================================================================

; (load "/p/nl/tools/ttt/src/load")  ; moved to "init.lisp"

(defparameter *infer-want-from-request*
;``````````````````````````````````````
; Obtain a want-inference from a polite request (top-level).
; TTT operator ^* could be used, but we control search separately.
; e.g., "Can somebody help me?"
;       (((pres can.aux-v) somebody.pro (help.v me.pro)) [?]) 
;       => "I want that somebody helps me"
;       (I.pro ((pres want.v) (that (somebody.pro (help.v me.pro)))))
; NB: ttt symbols like !, ?, ~, + etc., appearing in the ULF must
;     first be square-bracketed so as not to "confuse" ttt.
; How to apply: use ttt:apply-rule after square-bracketing '?' or
;     applying hide-ttt-ops (if necessary recovering them via 'unhide-ttt-ops')
   '(/ ((pres (! aux-indicating-request?))
        (!1 you.pro someone.pro somebody.pro)
        (? please.adv-s) _+)  ; will have to presubst for ?, !
       (I.pro ((pres want.v) (that (!1 _+))))))
  ; We'll want to potentially chain from the last part, if it enables
  ; further attitudinal, state change/cessation/inception/continuation
  ; or other inferences.

(defparameter *infer-expect-from-request*
;```````````````````````````````````````
; Obtain an expect-inference from a polite request (top-level).
; TTT operator ^* could be used, but we control search separately.
; e.g., "Can somebody help me?"  => "I expect that somebody help me"
;       (Note: The rule could be refined to yield "...will help me")
   '(/ ((pres (! aux-indicating-request?))
        (!1 you.pro someone.pro somebody.pro)
        (? please.adv-s) _+) 
       (I.pro ((pres expect.v) (that (!1 _+))))))


(defparameter *infer-falsehood-from-positive-counterfactual*
;``````````````````````````````````````````````````````````
; For a top-level counterfactual, e.g., "If I were rich ..." or "I wish
; I were rich".
;
; A simplification of *infer-falsehood-from-positive-counterfactual-if*,
; and *infer-falsehood-from-positive-counterfactual-wish*, not looking
; for the "if" or "wish", to allow e.g., for (if (only ...) ...);
; It'll also cover "John thinks that if he were rich ...", etc.,
; if we search through the ulf.
;
 '(/ ((! ~ adv-s)   (? atom? ~ not.adv-s not never.adv-f never.adv-s)
      ;subj         ```````` poss. nonneg. adv (as sibling of subj & VP
       ((!2 verb-cf?)
        (?1 atom? ~ not.adv-s not never.adv-f never.adv-s)
           ;``````````````` poss. nonneg. adv (sibling of head verb)
        _!1))
     (! (negate-vp! (non-cf-version! ((pres !2) ? ?1 _!1))))))


(defparameter *infer-falsehood-from-inverted-positive-counterfactual*
;```````````````````````````````````````````````````````````````````
; E.g., "Had I known your telephone number ..."
 '(/ ((!2 verb-cf?) _!
         (? atom? ~ not.adv-s not never.adv-f never.adv-s)
           ;``````````````` poss. nonneg. adv (sibling of head verb)
         _!1)
     (_! (negate-vp! (non-cf-version! ((pres !2) ? _!1))))))


(defparameter *infer-fact-from-negative-counterfactual*
;``````````````````````````````````````````````````````
; E.g., "You will wish you had never seen it"
;
 '(/ ((! ~ adv-s)   (? not.adv-s not never.adv-f never.adv-s)
      ;subj          ```````` poss. neg. adv (as sibling of subj & VP
       ((! verb-cf?)
        (!1 not.adv-s not never.adv-f never.adv-s)
           ;``````` neg. adv (sibling of head verb)
           ;        NB: same variable '?' as above (double-neg ~= neg)
        _!1))
     (! (non-cf-version! ((pres !) _!1)))))


(defparameter *infer-fact-from-inverted-negative-counterfactual*
;```````````````````````````````````````````````````````````````
; E.g. "Had I not met her, ..."
;
 '(/ ((! verb-cf?) _!
         (!1 not.adv-s not never.adv-f never.adv-s)
           ;``````````````` neg. adv (sibling of head verb)
         _!1)
      (_! (non-cf-version! ((pres !) _!1)))))


(defun aux-indicating-request? (x)
     (find x '(can.aux-v can.aux-s will.aux-a will.aux-s would.aux-v
               would.aux-s could.aux-v could.aux-s)))

(defun hide-ttt-ops (wff); tested
;~~~~~~~~~~~~~~~~~~~~~~~~
; Wrap [..] around symbols like !, +, ?, *, @, ~, {}, or <>, or
; ones starting this way, which we may want to use in some patterns
; (e.g., in wff-patterns involving *, **, @, or ~), but can't 
; because of their special meanings in TTT. We're assuming that
; the wffs we want to process don't *already* contain symbols in
; square brackets, starting as above inside the brackets, and which
; shouldn't have the brackets removed when we ultimately "unhide"
; the hidden symbols in a formula.
;
  (let (str chars)
       (cond ((symbolp wff)
              (setq str (string wff))
              (setq chars (coerce str 'list))
              (cond ((member (car chars) '(#\! #\+ #\? #\* #\@ #\~))
                     (intern (concatenate 'string "[" str "]")))
                    ((and (eq (car chars) #\{) (eq (second chars) #\}))
                     (intern (concatenate 'string "[" str "]")))
                    ((and (eq (car chars) #\<) (eq (second chars) #\>))
                     (intern (concatenate 'string "[" str "]")))
                    (t wff)))
             ((atom wff) wff)
             (t (cons (hide-ttt-ops (car wff)) (hide-ttt-ops (cdr wff)))))
 )); end of hide-ttt-ops


(defun unhide-ttt-ops (wff); tested
;~~~~~~~~~~~~~~~~~~~~~~~~~~
; Remove the square brackets that have been added around ttt symbols
; in wff by 'hide-ttt-ops':
;
 (let (str chars)
      (cond ((symbolp wff)
             (setq str (string wff))
             (setq chars (coerce str 'list))
             (cond ((or (not (eq (car chars) #\[))
                        (not (eq (car (last chars)) #\]))) wff)
                   (t (setq chars (cdr (butlast chars)))
                      (setq str (coerce chars 'string))
                      (cond ((null chars) wff)
                            ((member (car chars) '(#\! #\+ #\? #\* #\@ #\~))
                             (intern str))
                            ((and (eq (car chars) #\{) (eq (second chars) #\}))
                             (intern str))
                            ((and (eq (car chars) #\<) (eq (second chars) #\>))
                             (intern str))
                            (t wff)))))
            ((atom wff) wff)
            (t (cons (unhide-ttt-ops (car wff)) (unhide-ttt-ops (cdr wff)))))
 )); end of unhide-ttt-ops


(defun atom? (x) (atom x))
;````````````````````````


(defun verb-cf? (verb)
;`````````````````````
; were-cf.v, were-cf.aux-s, could-cf.aux-s, could-cf.v, had-cf.v, had-cf.aux-s
; are some of the counterfactual verbs (plus would-cf.aux-s, was-cf.v, was-cf.aux-s,
; and in principle any verb could be used counterfactually ("If I won million
; dollars, I would ..."). In all cases, the '-cf' is the give-away. The code
; allows zero or one digit after the -cf, followed by a dot or nothing (where
; the dot may have more characters after it). The digit may be needed for WSD.
;
 (if (not (symbolp verb)) (return-from verb-cf? nil))
 (let ((chars (member #\- (coerce (string verb) 'list))))
      (if (and chars (member (second chars) '(#\C #\c))
                     (member (third chars) '(#\F #\f))
                     (or (member (fourth chars) '(#\. nil))
                         (and (member (fourth chars) 
                               '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
                              (member (fifth chars) '(#\. nil)))))
          verb nil)))

(defun non-cf-version! (tensed-verb+comps); tested
;`````````````````````````````````
; NB: 'comps' can be a sequence, 
;     e.g., ((pres could-cf.aux-v) just.adv-v (leave.v (the.d room.n)))   
; NB: Assume that in ULFs for passive counterfactuals, like "I wish he were fired", 
;     we use (-cf (pasv ...)); that's because the "be" preceding a passive is
;     rendered only as a tense  -- but we're taking counterfactual verbs not
;     to have tense to begin with! So where did the tenses below come from?
;     They came from the embedding context; e.g., compare 
;        "I wish I were rich" => "I AM not rich"
;        "(In those days) I wished I were rich" => I WAS not rich.
;     But passive counterfactuals like "I wish he were fired" don't have a
;     natural past version. We would say "I wished he would be fired",
;     rather than "I wished he were fired"; at least, that's assumed below.
; CASES:
; ((pres were-cf.v) comps) -> ((pres be.v) comps) e.g., were happy -> is happy
; ((past were-cf.v) comps) -> ((past be.v) comps) e.g., were happy -> was happy
; ((pres prog-cf) comps) -> ((pres prog) comps)  e.g., were singing -> is singing
; ((past prog-cf) comps) -> ((past prog) comps)  e.g., were singing -> was singing
; ((pres -cf) comps) -> ((pres futr) comps)     e.g., were fired -> will be fired
; ((past -cf) comps) -> ((past futr) comps)     e.g., were fired -> would be fired
; ((pres could-cf.aux-v) comps) -> ((pres can.aux-v) comps); sim'ly ...aux-s, would
; ((past could-cf.aux-v) comps) -> ((past can.aux-v) comps); sim'ly ...aux-s, would
; ((pres had-cf.v) comps) -> ((pres have.v) comps)   had a car -> have a car
; ((past had-cf.v) comps) -> ((past have.v) comps)   had a car -> had a car
; ((pres perf-cf) comps) -> ((past do.aux-s) comps)  had known -> did know (actual)
; ((past perf-cf) comps) -> ((past perf) comps)    had known -> had known (actual)
; METHOD: 
;   Deal with the special case ((pres perf-cf) comps) first;
;      E.g., "If I had known this, I would have ..." => "I did not know this"
;   Do the rest case-by-case, keying on the operand of the tense operator.
;
 (if (or (atom tensed-verb+comps) (not (listp (first tensed-verb+comps))))
     (return-from non-cf-version! tensed-verb+comps))
 (let* ((tensed-verb (first tensed-verb+comps)) 
        (comps (cdr tensed-verb+comps))
        (tense (first tensed-verb)) 
        (verb (second tensed-verb)))
       (if (equal tensed-verb '(pres perf-cf)) 
           (cons '(past do.aux-s) comps)
           (case verb
                 (were-cf.v (cons `(,tense be.v) comps))
                 (prog-cf (cons `(,tense prog) comps))
                 (-cf (cons `(,tense futr) comps))
                 (could-cf.aux-v (cons `(,tense can.aux-v) comps))
                 (could-cf.aux-s (cons `(,tense can.aux-s) comps))
                 (would-cf.aux-v (cons `(,tense will.aux-v) comps))
                 (would-cf.aux-s (cons `(,tense will.aux-s) comps))
                 (had-cf.v (cons `(,tense have.v) comps))
                 (perf-cf (cons `(,tense perf) comps))
                 (t (cons `(pres ,(remove-cf! verb)) comps))))
                ;***       ^^^^^^^^^^^^^^^^^^^^^^^^ THIS GIVES THINGS LIKE
                ;            (PRES RAN.V), RATHER THAN (PRES RUN.V);
                ;          IT SHOULD BE FIXED, BUT CF-USE OF MAIN VERBS
                ;          OTHER THAN "WERE" AND "HAD" ARE FAIRLY RARE.
 )); end of non-cf-version!


(defun remove-cf! (verb-cf); tested
;``````````````````````````
; e.g., (remove-cf! 'walk-cf.v) --> WALK.V
 (if (not (symbolp verb-cf)) (return-from remove-cf! verb-cf))
 (let ((atoms (split-into-atoms verb-cf)))
      (fuse-into-atom
        (ttt:apply-rule '(/ (_* - (! C |c|) (! F |f|) _*1) (_* _*1)) atoms))))
          

(defun split-into-atoms (atm); tested
;````````````````````````````
; Useful for applying TTT to processing symbols as list of separate
; symbolic characters. Caveat: Watch out for {!,~,^,*,+,?} as part of 'atm'
; as they are specially interpreted by TTT.
 (mapcar #'intern 
    (mapcar #'(lambda (ch) (format nil "~a" ch)) 
              (coerce (string atm) 'list))))

(defun fuse-into-atom (atm-list); tested
;``````````````````````````````
; Make a single atom out of the list of atoms
; e.g., (fuse-into-atom '(this - and - that)) --> THIS-AND-THAT
 (intern (apply #'concatenate 'string (mapcar #'string atm-list))))

(defun negate-vp! (ulf-vp) ; tested
;`````````````````````````
; e.g., ((past leave.v) abruptly.adv-a (the.d house.n)) -->
;       ((past do.aux-s) not.adv-s abruptly.adv-a (the.d house.n))
; e.g., ((past be.v) seemingly.adv-s happy.a) -->
;       ((past be.v) not.aux-s seemingly.adv-s happy.a)
; e.g., ((past do.aux-v) abruptly.adv-a (leave.v (the.d house.n))) -->
;       ((past do.aux-v) not.adv-s abruptly.adv-a (leave.v (the.d house.n)))
; e.g., ((past have.v) (a.d car.n)) --> 
;       ((past do.aux-s) not.adv-s (have.v (a.d car.n)))
; e.g., ((pres can.aux-v) clearly.adv-a (see.v it.pro)) -->
;       ((pres can.aux-v) not.adv-s clearly.adv-a (see.v it.pro))
; e.g., ((pres futr) ((pasv fire.v (by-arg.p |Mary|)))) -->
;       ((pres futr) not.adv-s ((pasv fire.v (by-arg.p |Mary|))))
; e.g., ((past perf) certainly.adv-s (recognize.v her.pro)) -->
;       ((past perf) not.adv-s certainly.adv-s (recognize.v her.pro))
; e.g., ((pres perf) frequently.adv-f (meet.v her.pro)) -->
;       ((pres perf) not.adv-s frequently.adv-f (meet.v her.pro))
; So for main verbs exclusive of be.v we introduce do.aux-s.
;
; So we'll always put the negation right after the head verb, because
; it should get wider scope than other adv-s's -- even though this
; will sometimes sound unnatural, e.g., "He had certainly recognized her"
; --> ?"He had not certainly recognized her" (but correctly implies "It
; is not certain that he had recognized her". Similarly, "He perhaps
; recognized her" => ??"He did not perhaps recognize her" (which correctly
; implies "It is not possible that he recognized her".
; 
 (if (atom ulf-vp) 
     (return-from negate-vp! `(non.adv-a ,ulf-vp))); unexpected
 (let (tensed-verb comps tense verb)
      (if (member (first ulf-vp) '(pres past)) ; no complements?
          (setq tensed-verb ulf-vp comps nil)
          (setq tensed-verb (first ulf-vp) comps (cdr ulf-vp)))
          ; This makes the 2 cases (almost) uniformly processable
      (setq tense (first tensed-verb))
      (setq verb (second tensed-verb))
      (if (or (aux? verb) (eq verb 'be.v))
          (cons tensed-verb (cons 'not.adv-s comps))
          (cons `(,tense do.aux-s)
                 (cons 'not.adv-s 
                       (if comps (list (cons verb comps))
                                 (cons verb comps)))))
 )); end of negate-vp!

(defun aux? (verb); tested
;````````````````
; verb is auxiliary if it contains substring '.aux'
 (if (not (symbolp verb)) (return-from aux? nil))
 (let ((atoms (split-into-atoms verb)))
      (ttt:match-expr '(_* \. (! A \a) (! U \u) (! X \x _*)) atoms)))


