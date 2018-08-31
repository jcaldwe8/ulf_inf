;; NOV 4/17: THIS FILE HHAS BEEN COPIED FROM ~schubert/elf-from-sentences,
;;           FOR PARTIAL USE/MODIFICATION HERE.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; These are intended to override some of the pred definitions in
; /p/nl/tools/ttt/examples/caption-funcdefs.lisp; (at the time
; of writing, that file is loaded when
;    (load "/p/nl/tools/ttt/src/ttt")
; is executed, and contains definitions of PP?, PPT?, locative-p?,
; nn-human?, and nn-nonhuman?
;
; We also provide ttt-functions (with a name ending in an exclamation
; mark) needed for caption processing

; The initial set of preds and functions are syntactic, i.e., concerned
; with the content of parse trees.

(defun verb-cat? (atm)
; ~~~~~~~~~~~~~~~~~~~
; E.g., true of VB, VBZ, AUX, BE, etc.
; As noted in "initialize-and-preprocess.lisp", the isa-scheme used here
; makes V, AUX, and (copular) BE disjoint, so they need to be separately 
; checked for.
 (if (not (symbolp atm)) nil
     (or (isa atm 'v) (isa atm 'aux) (isa atm 'be))))

(defun aux-be? (pair)
;~~~~~~~~~~~~~~~~~~~
; e.g., (AUXD was); needed for changing progressive "be" to a PRG-form
;
 (and (not (atom pair)) 
      (member pair '((AUX be) (AUXZ am) (AUXZ is) (AUXP are) (AUXD was)
                         (AUXD were)) :test 'equal)))

(defun change-to-prog-be-form! (pair)
; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; E.g., change (AUXD was) to (PRGD was)
;
 (case  (car pair)
        (AUX `(PRG ,(second pair)))
        (AUXZ `(PRGZ ,(second pair)))
        (AUXP `(PRGP ,(second pair)))
        (AUXD `(PRGD ,(second pair)))
        (T pair))); end of change-to-prog-be-form!

(defun pp-cat? (atm)
; ~~~~~~~~~~~~~~~~~
; E.g., true of 'PP-ABOUT, 'PP-FROM, etc. The instances are from 
;    "/u/schubert/lf*/initialize-and-preprocess.lisp",
; augmented with some from A.P.'s code. If that file has been loaded, 
; then we can use the fast method below,
;  (defun PP-CAT? (atm) (if (symbolp atm) (eq (get atm 'isa) 'pp)))   
; or even
;  (defun pp-cat? (atm) (isa atm 'pp)) [I've changed to that for now...]
; instead of the listing that I've commented out.
 (if (not (symbolp atm)) nil 
     (isa atm 'pp)))
  ;  (find atm 
  ;  '(PP-ABOVE PP-ABOUT PP-ACROSS PP-AFTER PP-AGAINST PP-ALONG PP-AMONG
  ;    PP-AROUND PP-AS PP-AT PP-BEFORE PP-BEHIND PP-BELOW PP-BENEATH
  ;    PP-BESIDE PP-BETWEEN PP-BETWIXT PP-BEYOND PP-BY PP-DESPITE 
  ;    PP-DOWN PP-DURING PP-FOR PP-FROM PP-IN PP-INSIDE PP-INTO PP-LIKE 
  ;    PP-NEAR PP-NOTWITHSTANDING PP-OF PP-OFF PP-ON PP-ONTO PP-OUTSIDE
  ;    PP-OUT-OF PP-OVER PP-SINCE PP-THROUGH PP-THROUGHOUT PP-TO PP-TOWARD
  ;    PP-UNDER PP-UNDERNEATH PP-UNTIL PP-UP PP-UPON PP-WITH PP-WITHIN
  ;    PP-WITHOUT PP-TIME ))))

(defun pp-time-cat? (atm)
; ~~~~~~~~~~~~~~~~~~~~~~
  (isa atm 'pp-time))

(defun postmodifying-pp-cat? (atm)
; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; E.g., PP-ABOUT, PP-BY, PP-OF, ..., i.e., PPs that should stay attached
;    to the smallest NP on their immediate left, rather than being 
;    "raised" to modify a larger NP or other phrase
 (if (not (symbolp atm)) nil
     (find atm '(PP-ABOUT PP-AGAINST PP-BY PP-FOR PP-FROM PP-INTO 
       PP-OF PP-OFF PP-ONTO PP-OUT-OF PP-OVER PP-THROUGH PP-TO))))

(defun loc-p-tree? (expr)
; ~~~~~~~~~~~~~~~~~~~~~~~
; E.g., true of (IN at), (IN by), (IN above), etc.
;
 (if (atom expr) nil
     (and (eq (car expr) 'IN)
          (member (second expr)
           '(ABOVE ACROSS AGAINST ALONG AROUND AT BEHIND BELOW BENEATH
             BESIDE BETWEEN BETWIXT BEYOND BY DOWN IN INSIDE NEAR 
             ON OUTSIDE OVER THROUGH THROUGHOUT UNDER UNDERNEATH UP
             UPON WITH WITHIN)))))

(defun pp-tree? (expr); prepositional phrase (as tree)
; ~~~~~~~~~~~~~~~~~~~~
; E.g., true of (PP-ABOVE (IN above) (NP (NNP Rome)))
;
 (if (atom expr) nil
     (pp-cat? (car expr))))

(defun punc-tree? (x); e.g., (\. \.), (\: \;), (\: --)
;~~~~~~~~~~~~~~~~~~~~
 (and (listp x) (member (car x) '(\, \. \:))))


(defun ends-in-ly? (x)
;~~~~~~~~~~~~~~~~~~~~~
; Check if x is an atom ending in -ly (applied only to adverbs,
; i.e., RBs; could have used 'string-ends-in' function.
  (and (symbolp x)
       (equal (last (coerce (string x) 'list) 2) '(#\L #\Y))))

(defun sentence-modifying-advp? (advp)
; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; Decide on the bases of word content whether the given advp is
; a sentence-modifying (as opposed to predicate-modifying) one.
; Take for granted that the initial list element is ADVP, so we
; just look at constituent patterns.
;
 (or (ttt:match-expr '((RB (! not n\'t never))) (cdr advp))
     (ttt:match-expr 
       '((* (RB _!)) 
         (_! (! accordingly admittedly apparently assuredly certainly
                conceivably consequently definitely ever fortunately 
                frequently luckily maybe naturally obviously 
                occasionally of-course of_course often perhaps perchance 
                possibly potentially rarely reportedly sometimes 
                surprisingly undoubtedly unexpectedly unfortunately 
                unluckily unsurprisingly))) 
     (cdr advp))
     ; **any other patterns?? More complex ones?
  )); end of sentence-modifying-advp?
        

; **THIS IS A TASK-SPECIFIC STUB (FOR A SET OF PHOTOS), TO BE GENERALIZED
(defun nn-human? (expr)
;~~~~~~~~~~~~~~~~~~~~~~
; An ad hoc version for the 16 captions under consideration
; A better version will need to use gazetteers and perhaps some
; of the KNEXT functions for determining entity types.
;
 (cond ((atom expr) nil)
       ((and (member (car expr) '(NN NNS NNP NNPS))
             (member (second expr)
               '(Ben grandma lillian grandma-lillian Tanya
                 Josh Halle family families grandmas Mr\.
                 Mrs\. Weinberg Mordi uncle moshe Saul Klauser
                 coach basketball-coach Reggi sister kid kids
                 friend friends Noah Friedland Noah-Friedland)))
        t) (t nil))); end of nn-human?

; **THIS IS A TASK-SPECIFIC STUB (FOR A SET OF PHOTOS), TO BE GENERALIZED
(defun nn-nonhuman? (expr)
; ~~~~~~~~~~~~~~~~~~~~~~~
; A nominal that is not of the above type
;
 (cond ((atom expr) t)
       ((and (member (car expr) '(NN NNS NNP NNPS))
             (not (member (second expr)
                  '(ben grandma lillian grandma-lillian tanya
                    josh halle family families  grandmas mr\.
                    mrs\. weinberg mordi uncle moshe saul klauser
                    coach basketball-coach reggi sister kid kids
                    friend friends noah friedland noah-friedland))))
        t) (t nil))); end of nn-nonhuman?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; FROM THIS POINT ONE WE HAVE LF PREDS/FUNCTIONS RATHER THAN SYNTACTIC ONES
; `````````````````````````````````````````````````````````````````````````

(defun loc-pred? (atm)
; ~~~~~~~~~~~~~~~~~~~~
; E.g., true of at.p, by.p, with.p, above.p, etc.
;
 (if (listp atm) nil
     (member atm
      '(ABOVE.P ACROSS.P AGAINST.P ALONG.P AROUND.P AT.P BEHIND.P BELOW.P 
        BENEATH.P BESIDE.P BETWEEN.P BETWIXT.P BEYOND.P BY.P DOWN.P IN.P 
        INSIDE.P NEAR.P ON.P OUTSIDE.P OVER.P THROUGH.P THROUGHOUT.P 
        UNDER.P UNDERNEATH.P UP.P UPON.P WITH.P WITHIN.P))))

(defun numeral-pred? (x); e.g., 'four.a'
; ~~~~~~~~~~~~~~~~~
 (if (not (symbolp x)) 
     nil ; ** could be generalized to complex numerals
     (case x 
       ((two.a three.a four.a five.a six.a seven.n eight.a nine.a
         ten.a eleven.a twelve.a thirteen.a fourteen.a fifteen.a
         sixteen.a seventeen.a eighteen.a nineten.a twenty.a) x)
       (t nil)) ; ** obviously not perfectly general!
 )); end of numeral-pred?

(defun relnom? (x)
; ~~~~~~~~~~~~~~~~
; Is x a relational nominal predicate such as friend.n, mother.n, etc.?
  (if (and (symbolp x)
       (case x
        ((; interpersonal relationships
          grandson.n son.n brother.n father.n dad.n daddy.n grandfather.n
          grandpa.n granddad.n gramps.n nephew.n cousin.n uncle.n
          granddaughter.n daughter.n sister.n mother.n mom.n mommy.n
          grandmother.n grandma.n grandmom.n granny.n niece.n aunt.n 
          auntie.n spouse.n husband.n wife.n friend.n boyfriend.n 
          girlfriend.n partner.n fiance.n fiancee.n boss.n employer.n
          employee.n room-mate.n office-mate.n coworker.n coach.n
          basketball-coach.n football-coach.n baseball-coach.n
          hockey-coach.n soccer-coach.n swimming-coach.n tennis-coach.n
          gymnastics-coach.n wrestling-coach.n volleyball-coach.n
          ; some object-functions
          end.n beginning.n start.n middle.n surface.n weight.n age.n
          length.n height.n width.n diameter.n circumference.n girth.n
          interior.n exterior.n volume.n population.n contents.n area.n
          top.n bottom.n side.n back.n front.n rest.n remainder.n
          ; some milestone personal events
          graduation.n wedding.n engagement.n funeral.n 
          birth.n birthday.n bar-mitzvah.n bat-mitzvah.n 
          win.n loss.n victory.n triumph.n induction.n introduction.n
          appointment.n promotion.n election.n
         ) t))) t)
 ); end of relnom?

(defun append-of! (x)
; ~~~~~~~~~~~~~~~~~~~
; Insert '-of' prior to the dot-extension of x (or just append it
; if there is no dot-extension); the use of subroutines entails
; breaking the symbol into a character list multiple times, but
; this minor inefficiency shouldn't matter.
;
  (cond ((not (symbolp x)) x)
        ((lacks-dot-extension x) 
         (intern (coerce 
                    (append (coerce (string x) 'list) '(#\- #\O #\F))
                    'string)))
        (t (intern (coerce
                    (append (coerce (string-minus-dot-extension x) 'list)
                            '(#\- #\O #\F) (dot-extension-as-list x))
                    'string)))
 )); end of append-of!

; (defun quan? ...) not included here -- it is defined in 
; /u/schubert/lf*/logical-form-predicates.lisp
; However, unless quantifiers are type-flagged with values like
; det, dt, d, or quan, only quantifiers on the following explicit
; list are recognized: '(all some a{n} most few many the this these)

; (defun coord? ...) likewise is defined in
; /u/schubert/lf*/logical-form-predicates.lisp, and covers the
; following:
; '(and and-then as-well-as but or or-else either-or
;   => implies <=> equiv would-imply because so-that)

; (defun new-skolem! ...) is defined in ./process-sentence.lisp
;                         (originally  ./process-and-align-caption.lisp)


; (defun subst! (x y z) (subst x y z)); synonym -- defined this way in 
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  ; ./process-and-align-caption.lisp 
; N.B.: It is assumed that 'expr' contains only UNBOUND occurrences
; of x.

(defun skolem? (x)
;~~~~~~~~~~~~~~~~
; Check if x a skolem constant, i.e., with extension .SK;
; could have used 'string-ends-in' function.
  (and (symbolp x)
       (equal (last (coerce (string x) 'list) 3) '(#\. #\S #\K))))

(defun male-skolem? (x)
;~~~~~~~~~~~~~~~~~~~~~
; e.g., uncle3.sk (as constructed in Skolemizing "uncle" in something 
; like "John and his uncle"). Return the type, e.g., 'DAD', 'GRANDPA'
; (can use in asserting name-based facts or in forward TTT rules).
;
  (let (lst sym)
    (and (symbolp x)
         (setq lst (coerce (string x) 'list))
         (equal (last lst 3) '(#\. #\S #\K))
         (setq lst (butlast lst 3)); strip .sk and (next line) final digits
         (setq lst (reverse (member-if-not #'digit-char-p (reverse lst))))
         (setq sym (intern (coerce lst 'string)))
         (find sym '(father dad grandfather granddad grandpa son
                       grandson brother bro uncle nephew 
                       boyfriend husband groom coach)))
                   ; currently these are all relational, but we could
                   ; also have 'guy', 'fellow', 'man', 'boy', 'shaman', etc.
 )); end of male-skolem?

(defun female-skolem? (x)
;~~~~~~~~~~~~~~~~~~~~~~~~
; e.g., aunt3.sk (as constructed in Skolemizing "aunt" in something 
; like "John and his aunt"). Return the type, e.g., 'MOTHER', 'GRANDMA'
; (can use in asserting name-based facts or in forward TTT rules).
;
  (let (lst sym)
    (and (symbolp x)
         (setq lst (coerce (string x) 'list))
         (equal (last lst 3) '(#\. #\S #\K))
         (setq lst (butlast lst 3)); strip .sk and (next line) final digits
         (setq lst (reverse (member-if-not #'digit-char-p (reverse lst))))
         (setq sym (intern (coerce lst 'string)))
         (find sym '(mother mom grandmother grandma granny daughter
                       granddaughter sister sis aunt auntie niece 
                       girlfriend bride wife)))
 )); end of female-skolem

(defun unknown-gender-skolem? (x)
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; e.g., friend3.sk (as constructed in Skolemizing "friend" in something 
; like "John and his friend"). Return the prefix, e.g., 'FRIEND', 'CHILD'
; (can use in asserting name-based facts or in forward TTT rules).
;
  (let (lst sym)
    (and (symbolp x)
         (setq lst (coerce (string x) 'list))
         (equal (last lst 3) '(#\. #\S #\K))
         (setq lst (butlast lst 3)); strip .sk and (next line) final digits
         (setq lst (reverse (member-if-not #'digit-char-p (reverse lst))))
         (setq sym (intern (coerce lst 'string)))
         (case sym 
           ((child kid baby student person neighbor teacher soldier 
             flight-attendant cop grandson son brother father dad daddy
             grandfather grandpa granddad gramps nephew cousin uncle
             granddaughter daughter sister mother mom mommy grandmother
             grandma grandmom granny niece aunt auntie spouse husband 
             wife friend boyfriend girlfriend partner fiance fiancee
             boss employer employee room-mate office-mate coworker coach
             basketball-coach football-coach baseball-coach hockey-coach
             soccer-coach swimming-coach tennis-coach gymnastics-coach
             wrestling-coach volleyball-coach) sym)
                   ; ** numerous human types and role designators could
                   ;    and eventually should be added
           (t nil)))
 )); end of unknown-gender-skolem?

(defun name? (x)
; ~~~~~~~~~~~~~
; Check if x is a name, i.e., with extension .NAME; could have used
; 'string-ends-in' function.
  (and (symbolp x)
       (equal (last (coerce (string-upcase x) 'list) 5) 
             '(#\. #\N #\A #\M #\E))))

(defun male-name? (x)
; ~~~~~~~~~~~~~~~~~~~
; This is true for known male names like 'BEN.NAME. We rely on
; "/u/schubert/lf-from-treebank-trees/initialize-and-preprocess.lisp" 
; having been loaded (thus making use of the "male-names.lisp" gazetteer);
; Also allow for titular names , e.g., 'Grandpa_Jones.name, 'Mr._Jones.name
  (or (and (symbolp x) (eq (get x 'entity-type) 'male*.n))
      (titled-male-name? x)))

(defun titled-male-name? (x)
;~~~~~~~~~~~~~~~~~~~~~~~~~~
; e.g., 'Grandpa_Jones.name, 'Mr._Jones.name, 'Daddy_Warbucks
; Return the title, e.g., 'Mr.', 'Daddy' (usable in forward TTT rules)
 (let (chars suffix prefix)
      (if (not (symbolp x)) (return-from titled-male-name? nil))
      (setq chars (coerce (string x) 'list))
      (setq suffix (member #\_ chars))
      (if (null suffix) (return-from titled-male-name? nil))
      (setq prefix (butlast chars (length suffix)))
      (setq prefix (intern (coerce prefix 'string)))
      (find prefix '(mr mr. senor sir daddy grandfather granddad 
                     grandpa uncle father rabbi imam))
            ; more could be added from KNEXT: king, emperor, sultan,
            ; prince, sheik, raja, maharaja, baron, lord, duke, earl,
            ; marquis, count, brother, bishop, monsignor, pope, imam, 
            ; caliph, mullah.
 )); end of titled-male-name?

(defun female-name? (x)
; ~~~~~~~~~~~~~~~~~~~~~
; True for known female names like 'TANYA.NAME. We rely on
; "/u/schubert/lf-from-treebank-trees/initialize-and-preprocess.lisp" 
; having been loaded (thus making use of the "female-names.lisp" gazetteer)
; Also allow for titular names , e.g., 'Grandma_Lil.name, 'Mrs._Jones.name
  (or (and (symbolp x) (eq (get x 'entity-type) 'female*.n))
      (titled-female-name? x)))
   
(defun titled-female-name? (x)
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; e.g., 'Grandma_Lil.name, 'Mrs._Jones.name;
; Return the title, if found, e.g., 'GRANDMA', 'MRS.' (usable for inference)
(let (chars suffix prefix)
      (if (not (symbolp x)) (return-from titled-female-name? nil))
      (setq chars (coerce (string x) 'list))
      (setq suffix (member #\_ chars))
      (if (null suffix) (return-from titled-female-name? nil))
      (setq prefix (butlast chars (length suffix)))
      (setq prefix (intern (coerce prefix 'string)))
      (find prefix '(mrs mrs. ms ms. senora miss senorita lady madam 
                     grandmother grandma aunt auntie))
            ; more could be added from KNEXT: queen, empress, princess,
            ; baroness, duchess, countess, sister
 )); end of titled-female-name?


(defun titled-unknown-gender-name? (x)
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; e.g., Dr._Jones, Mayor_Ritchie
; Return the title, if found, e.g., 'DR.', 'MAYOR' (usable for inference)
(let (chars suffix prefix)
      (if (not (symbolp x)) (return-from titled-unknown-gender-name? nil))
      (setq chars (coerce (string x) 'list))
      (setq suffix (member #\_ chars))
      (if (null suffix) (return-from titled-unknown-gender-name? nil))
      (setq prefix (butlast chars (length suffix)))
      (setq prefix (intern (coerce prefix 'string)))
      (find prefix '(dr dr. doctor prof prof. professor coach rep
                     rep. hon hon. representative dir dir. director
                     justice judge mayor gen gen. general ;often male...
                     capt capt. captain gov gov. governor
                     pres pres. president vice-president
                     ambassador ceo)); might add others
 )); end of titled-unknown-gender-name?


(defun neuter-name? (x)
; ~~~~~~~~~~~~~~~~~~~~~
; True for known neuter names like 'CHICAGO.NAME. We rely on
; "/u/schubert/lf-from-treebank-trees/initialize-and-preprocess.lisp" 
; having been loaded (thus making use of the "non-personal-names.lisp" 
; gazetteer)
;
 (and (symbolp x)
      (member (get x 'entity-type)
       '(continent*.n country*.n us-state*.n us-city*.n
         world-city*.n government-agency*.n legislature*.n
         international-political-entity*.n activist-organization*.n
         service-agency*.n criminal-organization*.n weekday*.n
         month*.n religious-holiday*.n civic-holiday*.n
         planet*.n river*.n company*.n tv-network*.n martial-art*.n
         beverage*.n medicine*.n literary-work*.n))
 )); end of neuter-name?

(defun plur-name? (x)
;~~~~~~~~~~~~~~~~~~~
; True for plural names like Northern_Lights.name or The_Netherlands.name
; (if anything like that ever occurs; more likely we'll need to deal
; non-atomic, unscoped plurals like (:Q THE.DET (:F CONSTRUE-AS-PRED 
; SMITHS.NAME)))
 nil ); **STUB!!


(defun unknown-gender-name? (x)
; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; Note: the negations mean "not recognized as", since name-knowledge
; can't be assumed to be complete. Typically a T result means that
; x names a human.
;
 (and (symbolp x) (name? x) (not (male-name? x)) 
      (not (female-name? x)) (not (neuter-name? x))))

(defun female-constant? (x) 
;~~~~~~~~~~~~~~~~~~~~~~~~~
  (or (female-name? x) (female-skolem? x)))

(defun male-constant? (x) 
;~~~~~~~~~~~~~~~~~~~~~~~~
  (or (male-name? x) (male-skolem? x)))

(defun unknown-gender-constant? (x)
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  (or (unknown-gender-name? x) (unknown-gender-skolem? x)))

(defun pron? (x)
; ~~~~~~~~~~~~~
; Check if x is a pronoun, i.e., with extension .PRO or .PRON; 
; Note that in LF-formation, pronouns are rendered without case,
; and are given a numeric plus .PRO or .PRON extension (both
; versions are allowed because it's hard to remember which one
; is "correct"); e.g., an occurrence of "him" might have LF 
; HE3.PRO and an occurrence of "them" might have LF THEY32.PRO.
;
 (and (symbolp x)
      (let ((str (string-upcase x)))
       (or (string-ends-in str ".PRO") (string-ends-in str ".PRON")))))

(defun male-pron? (x)
; ~~~~~~~~~~~~~~~~~~~
 (and (pron? x) 
      (let ((chars (coerce (string-upcase x) 'list)))
           (and (char= (first chars) #\H)
                (char= (second chars) #\E)))
 )); end of male-pron?


(defun female-pron? (x)
; ~~~~~~~~~~~~~~~~~~~~~
 (and (pron? x) 
      (let ((chars (coerce (string-upcase x) 'list)))
           (and (char= (first chars) #\S)
                (char= (second chars) #\H)
                (char= (third chars) #\E)))
 )); end of female-pron?


(defun neuter-pron? (x)
; ~~~~~~~~~~~~~~~~~~~~~
; Singular neuter pronoun, i.e., "it" plus perhaps digits plus .pro(n)
; We're neglecting "this" and "that", as these typically have 
; propositional or situation/event reference (to be treated in future,
; calling for a 'demo-pron?' or 'sing-demo-pron?' predicate); the 
; plural versions, "these" and "those", also remain to be treated.
 (and (pron? x) 
      (let ((chars (coerce (string-upcase x) 'list)))
           (and (char= (first chars) #\I)
                (char= (second chars) #\T)))
 )); end of neuter-pron?


(defun plur-pron? (x)
; ~~~~~~~~~~~~~~~~~~~
; THEY plus digit(s) plus .PRO or .PRON; omitting "these" and 
; "those" for now
 (and (pron? x) 
      (let ((chars (coerce (string-upcase x) 'list)))
           (and (char= (first chars) #\T)
                (char= (second chars) #\H)
                (char= (third chars) #\E)
                (char= (third chars) #\Y)))
 )); end of plur-pron?


(defun sing-pron? (x)
; ~~~~~~~~~~~~~~~~~~~
 (and (pron? x) (or (male-pron? x) (female-pron? x) (neuter-pron? x))))


(defun male-name-or-pron? (x)
; ~~~~~~~~~~~~~~~~~~~~~~~~~~~
 (or (male-name? x) (male-pron? x)))


(defun female-name-or-pron? (x)
; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 (or (female-name? x) (female-pron? x)))


(defun neuter-name-or-pron? (x)
; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 (or (neuter-name? x) (neuter-pron? x)))

(defun plural-name-or-pron? (x)
; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 (or (plural-name? x) (plural-pron? x))); ** 'plural-name?' IS A STUB

(defun adj-as-adv? (x)
; e.g., 'OUT.ADV', derived for "out playing in the snow" is really
; and adjectival pred; what others are there? Maybe "up making noises",
; "back making trouble", etc.?
 (member x '(out.adv up.adv in.adv back.adv)))

(defun adj-from-adv! (x)
; change .adv extension to .a
 (intern (coerce (butlast (coerce (string x) 'list) 2) 'string)))

; We also need 'verbal-pred?', but that is defined in ~schubert/lf-from*/*lisp

(defun intrans-verbal-pred? (x); ones particularly relevant to photos, for now
 (member x '(play.v sit.v stand.v lie.v relax.v pose.v rest.v swim.v
             sight-see.v sightsee.v hike.v party.v celebrate.v walk.v
             sprawl.v crouch.v sing.v cavort.v dance.v recline.v
             fool-around.v horse-around.v unwind.v loiter.v)))

(defun cognition-pred2? (x); transitive cognition predicates, object-control
; verbal preds like see.v, imagine.v, hear.v, ...
 (member x '(see.v spot.v observe.v watch.v glimpse.v notice.v spy.v
             hear.v feel.v ignore.v disregard.v forget.v mind.v imagine.v
             ponder.v consider.v)))


(defun likely-unpunctuated-acronym? (atm)
; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; Acronyms like MVP, NLP, BA, MD, PhD, PC, etc., that are fairly common
; but not common enough to be adequately covered by training on standard
; corpora, are apt to throw off the Charniak parser. This becomes less
; likely if periods are added to the letters. (Down side: acronyms that
; the parser knows about may not be handled as well once puctuated.) 
;
; The ones here were obtained from an international student handbook:
; www.agts.edu/community/international_student_handb.pdf
;
  (or (find atm '(ID LA BA |PhD| PO UFO VIP)); some common ones, not all 
                                               ; upper-case consonants
      (let ((chars (coerce (string atm) 'list)))
           ; fast check for unlikely acronyms
           (if (or (lower-case-p (car chars)) (> (length chars) 4))
               nil
               ; more careful check: all upper case consonants?
               (not (find-if
                    #'(lambda (ch) 
                        (or (not (upper-case-p ch))
                            (member ch '(#\A #\E #\I #\O #\U #\Y))))
                      chars))))
 )); end of likely-unpunctuated-acronym?

(defun punctuate-acronym! (atm)
; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; atm: a short upper-case atom
; Insert a period before each internal upper-case letter, and at the end;
; E.g., VIP becomes V.I.P.; |PhD| becomes |Ph.D.|
  (let ((chars (coerce (string atm) 'list)) result)
       (push (pop chars) result)
       (dolist (ch chars)
               (if (upper-case-p ch) (push #\. result))
               (push ch result) )
       (push #\. result)
       (intern (coerce (reverse result) 'string))
 )); end of punctuate-acronym!

(defun {in}definite-det? (x); Nov 18/13
; ~~~~~~~~~~~~~~~~~~~~~~~~~~
 (or (definite-det? x) (indefinite-det? x)))


(defun definite-det? (x); allow for variants in identifying definite determiners 
; ~~~~~~~~~~~~~~~~~~~~~~ 
 (cond ((null x) nil)
       ((listp x) t); seems to be a DETP -- a likely definite (** improve!!)
       (t (case x ((the the.d the.dt the.det) t)
                ; Note: "my", "this", "those", etc., are expected to be interpreted
                ; in terms of 'the', so they needn't be included here
                  (t nil))))); end of definite-det?

(defun indefinite-det? (x); allow for variants in identifying indefinite determiners 
; ~~~~~~~~~~~~~~~~~~~~~~~~ 
 (if (or (null x) (not (symbolp x))) 
     nil
     (case x ((some a an many a-few exist exists 
               some.d a.d an.d many.d a-few.d exist.d exists.d 
               some.dt a.dt an.dt many.dt a-few.dt exist.dt exists.dt
               some.det a.det an.det many.det a-few.det exist.det exists.det) t)
             (t nil)))); end of indefinite-det?

(defun indefinite-num-det? (x);
; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  (if (or (null x) (not (symbolp x)))
     nil
     (case x ((many many.d many.dt many.det a-few a-few.d a-few.dt a-few.det
               two two.d two.dt two.det 2.d 2.dt 2.det
               three three.d three.dt three.det 3.d 3.dt 3.det
               four four.d four.dt four.det 4.d 4.dt 4.det
               five five.d five.dt five.det 5.d 5.dt 5.det
               six six.d six.dt six.det 6.d 6.dt 6.det
               seven seven.d seven.dt seven.det 7.d 7.dt 7.det
               eight eight.d eight.dt eight.det 8.d 8.dt 8.det
               nine nine.d nine.dt nine.det 9.d 9.dt 9.det
               ten ten.d ten.dt ten.det 10.d 10.dt 10.det
               eleven eleven.d eleven.dt eleven.det 11.d 11.dt 11.det
               twelve twelve.d twelve.dt twelve.det 12.d 12.dt 12.det
               thirteen thirteen.d thirteen.dt thirteen.det 13.d 13.dt 13.det
               fourteen fourteen.d fourteen.dt fourteen.det 14.d 14.dt 14.det
               fifteen fifteen.d fifteen.dt fifteen.det 15.d 15.dt 15.det
               sixteen sixteen.d sixteen.dt sixteen.det 16.d 16.dt 16.det
               seventeen seventeen.d seventeen.dt seventeen.det 17.d 17.dt 17.det
               eighteen eighteen.d eighteen.dt eighteen.det 18.d 18.dt 18.det
               nineteen nineteen.d nineteen.dt nineteen.det 19.d 19.dt 19.det
               twenty twenty.d twenty.dt twenty.det 20.d 20.dt 20.det
               thirty thirty.d thirty.dt thirty.det 30.d 30.dt 30.det
               forty forty.d forty.dt forty.det 40.d 40.dt 40.det
               fifty fifty.d fifty.dt fifty.det 50.d 50.dt 50.det
               sixty sixty.d sixty.dt sixty.det 60.d 60.dt 60.det
               seventy seventy.d seventy.dt seventy.det 70.d 70.dt 70.det
               eighty eighty.d eighty.dt eighty.det 80.d 80.dt 80.det
               ninety ninety.d ninety.dt ninety.det 90.d 90.dt 90.det
               a-hundred a-hundred.d a-hundred.dt a-hundred.det 100.d 100.dt 100.det
               a-thousand a-thousand.d a-thousand.dt a-thousand.det 1000.d 
                                                      1000.dt 1000.det
               a-million a-million.d a-million.dt a-million.det 1000000.d 
                                                      1000000.dt 1000000.det) t)
             (t nil)) )); end of indefinite-num-det?

(defun adj-from-det! (x); currently designed for atomic determiners only
; ~~~~~~~~~~~~~~~~~~~~~
; From an indefinite numeric determiner such as 'many', 'many.det', 'a-few',
; 'a-few.d', 'five', 'five.dt', '5.d', '5.dt', or '5.det', derive the
; corresponding adjectival predicate, such as 'many.a', 'a-few.a', 'five.a',
; or '5.a'.
  (let (chars suffix-chars)
    (if (or (null x) (not (symbolp x))) 
        nil
        (progn (setq chars (coerce (format nil "~s" x) 'list))
               (setq suffix-chars (member #\. chars))
               (if suffix-chars (setq chars (butlast chars (length suffix-chars))))
               (intern (coerce (append chars '(#\. #\A)) 'string))))
 )); end of adj-from-det!


(defun change-to-adj-cat! (adv-cat)
; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; E.g, Change RB to JJ, RBR to JJR, and RBS to JJS;
; This is used e.g., in changing (RB well) to (JJ well), in the
; context "I am well" (where "well" is misparsed as an ADVP)
  (case adv-cat (RB 'JJ) (RBR 'JJR) (RBS 'JJS)))

; CURRENTLY UNUSED
(defun if-sk1=sk2-then-name-else-sk2! (sk1 sk2 name)
; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 (if (eq sk1 sk2) name sk2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; ; Two tests (from previous work):
; (apply-rule *drop-decl-speech-act* 
;  '(EXISTS U0 (U0 SAME-TIME NOW0)
;    ((SPEAKER TELL.V HEARER
;      (THAT
;       (EXISTS E0 (E0 AT-ABOUT NOW0)
;        (THE.DET Y ((Y DAD.N) AND.CC (NATHANIEL.NAME HAVE.V Y))
;         (PROG ((COLL-OF NATHANIEL.NAME Y) PLAY.V (K CATCH.N))
;         ** E0)))))
;     ** U0)))
; 
; (apply-rule *skol-rule*
;   '(EXISTS E0 (E0 AT-ABOUT NOW0)
;        (THE.DET Y ((Y DAD.N) AND.CC (NATHANIEL.NAME HAVE.V Y))
;         (PROG ((COLL-OF NATHANIEL.NAME Y) PLAY.V (K CATCH.N))
;         ** E0))))
  
(defun aux-indicating-request? (x)
     (find x '(can.aux-v can.aux-s will.aux-a will.aux-s would.aux-v
               would.aux-s could.aux-v could.aux-s)))

(defun atom? (x) (atom x))
;````````````````````````

(defun add-vp-tense! (vp+ tense)
;````````````````````````````
; Adds the given tense marker to the given verb phrase.
; Recursively search for first *.v or *.aux in depth first search.
;
; The first argument is a list of a verb phrase plus additional
; phrases that are wrapped around the vp at the end.
;
; (run.v) past -> (past run.v)
; ((run.v (k home.n))) past -> ((past run.v) (k home.n))
; (((call.v again.adv-a) later.adv-e)) pres
;   -> (((pres call.v) again.adv-a) later.adv-e)
  (labels 
    ((rechelper (vp)
       (cond 
         ;; Base case: found the verb/aux -- add tense and return.
         ((and (atom vp) (verbaux? vp)) (list tense vp))
         ;; Base case: other atom, simply return value.
         ((atom vp) vp)
         ;; Recursive case:
         ;;  recurse left, 
         ;;    if returned val is diff, reconstruct and return
         ;;    else recurse to right.
         (t 
           (let ((leftrec (rechelper (car vp))))
             (if (not (equal leftrec (car vp)))
               (cons leftrec (cdr vp))
               (cons (car vp) (rechelper (cdr vp))))))))
     ) ; end of labels definitions.
   
    ;; Main body.
    (let ((tvp (rechelper (car vp+)))
          (additional (cdr vp+)))
      (reduce #'list additional :initial-value tvp))))
          
    

(defparameter *stative-verbs*
  '(be.v have.v prog))

(defun stative-head-verb? (verb)
;````````````````````````
; Returns true if 'verb' as a head verb would make the VP stative.
; e.g. be.v -> t
;      make.v -> nil
; be.v, have.v, progressives, and auxiliaries all make stative VPs.
  (or (if (member verb *stative-verbs*) t)
      (aux? verb)))

(defun non-cf-version! (tensed-verb+comps); tested
;`````````````````````````````````
; NB: 'comps' can be a sequence, 
;     e.g., ((cf can.aux-v) just.adv-v (leave.v (the.d room.n)))   
; NB: Assume that in ULFs for passive counterfactuals, like "I wish he were fired", 
;     we use (cf (pasv ...)); that's because the "be" preceding a passive is
;     rendered only as a tense  -- but we're taking counterfactual verbs not
;     to have tense to begin with! So where did the tenses below come from?
;     They came from the embedding context; e.g., compare 
;        "I wish I were rich" => "I AM not rich"
;        "(In those days) I wished I were rich" => I WAS not rich.
;     But passive counterfactuals like "I wish he were fired" don't have a
;     natural past version. We would say "I wished he would be fired",
;     rather than "I wished he were fired"; at least, that's assumed below.
; PRESENT TENSE CASES:
; ;; Statives get a present tense reading.
; ;; For now, statives are: be.v, have.v, prog, *.aux
; ((cf were.v) comps) -> ((pres be.v) comps) e.g., were happy -> is happy
; ((cf prog) comps) -> ((pres prog) comps)  e.g., were singing -> is singing
; ((cf have.v) comps) -> ((pres have.v) comps)   had a car -> have a car
; ((cf can.aux-v) comps) -> ((pres can.aux-v) comps); sim'ly ...aux-s, would
; 
; ;; Telic verb phrases get a future reading.
; ;; Everything not classified as stative is assumed telic.
; ;; TODO: add temporal adverbial modiification of telicity (If I gave you money yesterday)
; ;; TODO: test passives
; ((cf (pasv <verb>)) comps) -> ((pres futr) comps)     e.g., were fired -> will be fired
; (cf ran) -> ((pres will.aux-s) run)   e.g., ran -> will run
; ((cf give.v) me.pro (k money.n)) -> ((pres will.aux-s) (give.v me.pro (k money.n))) 
;
; PAST TENSE CASES:
; ;; Everything is stative in the past tense.
; ((cf perf) (be.v comps)) -> ((past be.v) comps) e.g., had been happy -> was happy
; ((cf perf) (prog comps)) -> ((past prog) comps)  e.g., had been singing -> was singing
; ((cf can.aux-v) (perf comps)) -> ((past can.aux-v) comps);  could have run -> could run
;                                                             sim'ly ...aux-s, would
; ((cf perf) (have.v comps)) -> ((past have.v) comps)   had had a car -> had a car
; ((cf perf) comps) -> ((past do.aux-s) comps)  had known -> did know (actual)
;
; STATIVE WOULD: 
; ((cf will.aux-s) (be.v x)) -> ((pres be.v) x) ; would be in Rome -> am in Rome
;
; IMPLICIT PAST TENSE CASES (not yet handled -- needs more information to disambiguate):
; ((cf were.v) comps) -> ((past be.v) comps) e.g., were happy -> was happy
; ((cf prog) comps) -> ((past prog) comps)  e.g., were singing -> was singing
; ((cf (pasv <verb>)) comps) -> ((past futr) comps)     e.g., were fired -> would be fired
; ((cf can.aux-v) comps) -> ((past can.aux-v) comps); sim'ly ...aux-s, would
; ((cf had.v) comps) -> ((past have.v) comps)   had a car -> had a car
; ((cf perf) comps) -> ((past perf) comps)    had known -> had known (actual)
; METHOD: 
;   Deal with the special case ((pres perf-cf) comps) first;
;      E.g., "If I had known this, I would have ..." => "I did not know this"
;   Do the rest case-by-case, keying on the operand of the tense operator.
;
 (if *debug-ulf-inf*
   (format t "In non-cf-version!~%Argument: ~s~%" tensed-verb+comps))
 (if (or (atom tensed-verb+comps) (not (listp (first tensed-verb+comps)))
         (not (equal 'cf (caar tensed-verb+comps))))
     (return-from non-cf-version! tensed-verb+comps))
 (let* ((tensed-verb (first tensed-verb+comps)) 
        (comps (cdr tensed-verb+comps))
        (tense (first tensed-verb)) 
        (verb (second tensed-verb)))
   (cond
     ; past counterfactual
     ((eq 'perf verb)
      (add-vp-tense! (list (car comps)) 'past))
     ; subjunctive 'were'
     ((eq 'were.v verb) 
      (cons '(pres be.v) comps))
     ;; Look for 'perf' after the auxiliary.
     ;; 'If I could have gone home' -> 'I could (was able to) go home"
     ((and (listp (car comps)) 
           (not (null (car comps))) 
           (eq 'perf (caar comps))) 
      (cons `(past ,verb) (cdar comps)))
     ; stative would
     ; would be in Rome -> am in Rome
     ((and (eq 'will.aux-s verb) (listp (car comps)) 
           (not (null (car comps))) (stative-head-verb? (caar comps)))
      (add-vp-tense! (list (car comps)) 'pres))
     ; telic would
     ; would go home -> will go home
     ((eq 'will.aux-s verb)
      (cons `(pres ,verb) comps))
     ; basic stative 
     ((stative-head-verb? verb)
      (cons `(pres ,verb) comps))
     ; basic telic
     ; If I gave $5 -> I will give $5
     (t (list '(pres will.aux-s) (cons verb comps))))))

; TODO: this needs to be dealt with somewhere.  Maybe include a negate-cfvp!
; which negates VPs that are known to be in a CF context?  This way we can make the inference
; "give me" -> "will give me" (e.g. If you give me $5...).
; TELICITY BASED INFERENCE:
; "If you gave me five dollars" -/-> "You do not give me five dollars"
; Actually, it infers -> "You probably will not give me five dollars"
; ((cf give.v) me.pro y) -> (probably.adv-s ((pres will.aux-s) not.adv-s (give.v me.pro y)))
;

(defun negate-vp! (ulf-vp) ; tested
;`````````````````````````
; e.g., ((past leave.v) abruptly.adv-a (the.d house.n)) -->
;       ((past do.aux-s) not.adv-s (leave.v abruptly.adv-a (the.d house.n)))
; e.g., ((past be.v) seemingly.adv-s happy.a) -->
;       ((past be.v) not.aux-s seemingly.adv-s happy.a)
; e.g., ((past do.aux-v) abruptly.adv-a (leave.v (the.d house.n))) -->
;       ((past do.aux-v) not.adv-s abruptly.adv-a (leave.v (the.d house.n)))
; e.g., ((past have.v) (a.d car.n)) --> 
;       ((past do.aux-s) not.adv-s (have.v (a.d car.n)))
; e.g., ((pres can.aux-v) clearly.adv-a (see.v it.pro)) -->
;       ((pres can.aux-v) not.adv-s clearly.adv-a (see.v it.pro))
; e.g., ((pres futr) ((pasv fire.v (by-arg.p |Mary|)))) -->
;       (probably.adv-s ((pres futr) not.adv-s ((pasv fire.v (by-arg.p |Mary|)))))
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
; Negations of future tense will get a 'probably.adv-s' in addition to more
; explicitly reflect the modality of future tense.
; 
 (if *debug-ulf-inf*
   (format t "In negate-vp!~%Argument: ~s~%" ulf-vp))
 (if (atom ulf-vp) 
     (return-from negate-vp! `(non.adv-a ,ulf-vp))); unexpected
 (let (tensed-verb comps tense verb res)
      (if (member (first ulf-vp) '(pres past)) ; no complements?
          (setq tensed-verb ulf-vp comps nil)
          (setq tensed-verb (first ulf-vp) comps (cdr ulf-vp)))
          ; This makes the 2 cases (almost) uniformly processable
      (setq tense (first tensed-verb))
      (setq verb (second tensed-verb))
      ;; add 'do.aux-s' if verb is not 'be.v' or an auxiliary.
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
   (ttt:match-expr '(_* \. (! A \a) (! U \u) (! X \x) _*) atoms)))

(defun adv? (sym)
;````````````````
; symbol is an adverb if it contains the substring '.adv'
  (if (not (symbolp sym)) (return-from adv? nil))
  (let ((atoms (split-into-atoms sym)))
    (ttt:match-expr '(_* \. (! A \a) (! D \d) (! V \v) _*) atoms)))

(defun adv-s? (sym)
;````````````````
; symbol is an adv-s if it ends with the substring '.adv-s'
  (if (not (symbolp sym)) (return-from adv-s? nil))
  (let ((atoms (split-into-atoms sym)))
    (ttt:match-expr '(_* \. (! A \a) (! D \d) (! V \v) - (! S \s)) atoms)))


(defparameter *aux-like-operators*
  '(perf prog))

(defun aux-like? (verb) 
;`````````````````````
; verb is auxiliary like if it satisfies aux? or is a member of aux-like
; operators. 
  (or (aux? verb) (if (member verb *aux-like-operators*) t)))

(defun verb? (verb); tested
;````````````````
; symbol is a verb if it ends with '.v'
 (if (not (symbolp verb)) (return-from verb? nil))
 (let ((atoms (split-into-atoms verb)))
      (ttt:match-expr '(_* \. (! V \v)) atoms)))

(defun verbaux? (sym)
;```````````````````
; function for matching verbs or auxiliaries
  (or (verb? sym) (aux-like? sym)))

(defparameter *tense-symbols* '(past pres))
(defun tense? (sym)
  (if (member sym *tense-symbols*) t))

(defun remove-were-to! (were-to-embvp)
;``````````````````````````````````
; Takes a VP embedded within were-to and maps it to a semantically equivalent
; VP without 'were to', assuming that this VP is in an if-antecendent context.
; Assumes that the input is the cdr of infinitive VP.
;   e.g. 
;     Full sentence: '((If.ps (I.pro ((cf were.v) (to not.adv-s (go.v (to.p-arg (k sleep.n))))))) ...)
;     Input: '(not.adv-s (go.v (to.p-arg (k sleep.n))))
;     Output: '((cf do.aux-s) not.adv-s (go.v (to.p-arg (k sleep.n))))
;
; For clearly bad inputs, nil is returned.  Otherwise the behavior is undefined.
;
; Further examples (this function acts over the bracketed portions):
; If I were to [go to sleep] -> If I [went to sleep]
; If I were ever to [go to sleep] -> If I ever [went to sleep]
; If I were to [not go to sleep] -> If I [did not go to sleep]
; If I were to [not be a person] -> If I [were not a person]
;
  (if *debug-ulf-inf*
   (format t "In remove-were-to!~%Argument: ~s~%" were-to-embvp))
  (if (atom were-to-embvp) (return-from remove-were-to! nil))
  (labels
    (
     ;; Gets the negation operator within in two-layers deep.
     ;; Does depth-first search until it either gets more than two layers or
     ;; finds a negation operator.  Once a negation operator has been found,
     ;; the remaining recursion simply reconstructs the list.
     ;;
     ;; Returns (list without negation, negation operator)
     ;; The second piece is nil if not found.
     (get-negation (ulf depth)
       (cond
         ((> depth 2) (list ulf nil))
         ((member ulf '(not not.adv-s))
          (list nil ulf))
         ((atom ulf)
          (list ulf nil))
         (t
           (let (tmp)
             (setq tmp
                   (reduce
                     #'(lambda (acc new)
                         (let ((neg (second acc))
                               (ulf (first acc)))
                           (if neg
                             ;; Already found, just construct and continue.
                             (list (cons new ulf) neg)
                             ;; See if we can find a neg.
                             (let* ((recres (get-negation new (+ depth 1)))
                                    (recneg (second recres))
                                    (reculf (first recres)))
                               (if reculf
                                 (list (cons reculf ulf) recneg)
                                 (list ulf recneg))))))
                     ulf
                     :initial-value (list nil nil)))
             ;; Reverse the list, since it's built backwards by reduce.
             (list (reverse (first tmp)) (second tmp)))))
       ) ; end of get-negation

     ;; Flattens trivial recusive structures.
     ;; ((...)) -> (...).  
     (trivial-flatten (ulf)
       (cond
         ((atom ulf) ulf)
         (t 
           (let ((recres (mapcar #'trivial-flatten ulf)))
             (if (and (eql 1 (length ulf)) (listp (car ulf)))
                (car ulf)
                ulf))))
       ) ; end of trivial-flatten
     ) ; end of labels definitions

    ;; Main body.
    ;; 1. Preprocess out negation.
    (let* ((getnegres (get-negation were-to-embvp 1))
           (nn-were-to-embvp (first getnegres))
           (neg (second getnegres))
           (adv (if (= 2 (length nn-were-to-embvp)) (first nn-were-to-embvp)))
           (vp (car (last nn-were-to-embvp)))
           (verb (car vp)) 
           advvp negvp preadvvp)
      ;; GK: maybe just return the input so this is transparent if not used correctly?
      (if (< 2 (length nn-were-to-embvp)) (return-from remove-were-to! nil))

      (if *debug-ulf-inf*
        (format t "In remove-were-to!~%Argument: ~s~%nn-were-to-embvp: ~s~%neg: ~s~%adv: ~s~%" were-to-embvp nn-were-to-embvp neg adv))

      ;; add 'do.aux-s' if verb is not 'be.v' or an auxiliary-like thing,
      ;; add adverbial as necessary.
      (cond
        ;; Don't add do.aux-s.  Adv and neg go after verb.
        ((or (aux-like? verb) (eq verb 'be.v))
         (setq advvp (if (not (null adv)) (cons adv (cdr vp)) (cdr vp)))
         (setq negvp (if (not (null neg)) (cons neg advvp) advvp))
         (cons `(cf ,verb) negvp))

        ;; Add do.aux-s.  Adv and neg go after do.aux-s.
        ((not (null neg))
         (setq advvp (if (not (null adv)) (cons adv (list vp)) (list vp)))
         (setq negvp (cons neg advvp))
         (cons '(cf do.aux-s) negvp))

        ;; No neg, add vp as necessary before everything.
        (t
          (setq preadvvp (cons `(cf ,verb) (cdr vp)))
          (if (null adv)
            preadvvp
            (list adv preadvvp)))))))



(defparameter *ttt-flatten-adv-s*
  '(/ (_!1 ((!2 adv-s?) _+3) _*4)
      (_!1 !2 _+3 _*4)))
(defun flatten-adv-s (ulf)
;`````````````````````````
; Flattens all adv-s in a ULF.
;
; (x (*.adv-s y) z) -> (x *.adv-s y z)
  (ttt:apply-rule *ttt-flatten-adv-s* ulf))  

(defparameter *ttt-past-will-to-past-do*
  '(/ (past will.aux-s)
      (past do.aux-s)))
(defun past-will-to-past-do (ulf)
;```````````````````````````````
; Maps (past will.aux-s) to (past do.aux-s).
; In the context of making inference, the latter is almost always warrented
; from the former.  For example, if I say "he would realize his mistake" with
; a (past will.aux-s) interpretation, it is also true that "he realized his
; mistake".  Logically this is not sound since the modal would in the past
; allows for cases where the mistake has not yet been realized, but will be.
; In practice, however, "he would realize his mistake" is only used if the
; speaker knows that this has happened in between the reference time and the
; utterance time.
  (ttt:apply-rule *ttt-past-will-to-past-do* ulf))

(defparameter *ttt-remove-redundant-do-basic*
  '(/ ((tense? do.aux-s) _!1)
      (add-vp-tense! (_!1) tense?)))
(defparameter *ttt-remove-redundant-do-adv*
  '(/ ((tense? do.aux-s) _!1 (+ (!2 adv? 
                                    ((!3 adv-a adv-s adv-a) _!))))
      ((add-vp-tense! (_!1) tense?) +)))
(defun remove-redundant-do (ulf)
;```````````````````````````````
; Removes do.aux-s if followed directly by a verb, rather than, say a negation
; operator.
  (ttt:apply-rule *ttt-remove-redundant-do-adv*
                  (ttt:apply-rule *ttt-remove-redundant-do-basic* ulf)))

