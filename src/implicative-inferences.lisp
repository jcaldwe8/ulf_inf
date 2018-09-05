;;; Ben Kane 7-22-2018
;;;
;;; Parsed into TTT rules from Karl Stratos rules from Epilog package using /resources/stratos/parser.lisp.

;; TTT computable predicate; accepts tense/aspect operators in front of verb
;; NOTE: Can be restricted to just "past" and "pres"
(DEFUN TENSE? (L) (IF (MEMBER L '(PAST PRES PERF PROG)) T NIL))

;; Define class for implicative rules containing type ('S or 'S-PRS), polarity ('+ or '-) and the rule
(DEFCLASS IMPLICATIVE-RULE-TTT NIL
          ((TYPE :INITARG :TYPE :INITFORM 'S)
           (POLARITY :INITARG :POLARITY :INITFORM '+)
           (RULE :INITARG :RULE)))

(DEFPARAMETER *INFER-FROM-IMPLICATIVE-RULES*
;``````````````````````````````````````````````````````````
; For implicative rules, e.g. "John knows that Mary went to
; the game" => "Mary went to the game".
;
; Each rule is stored as a class. The following values can be accessed using 'slot-value':
; :TYPE      :  Whether or not rule is presuppositional ('S-PRS vs. 'S)
; :POLARITY  :  Whether a rule applies to a positive ('+) or negative ('-) polarity
; :RULE      :  The specific TTT rule for a particular implicative verb
;
    (LIST
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(/ (_!1
                         ((! ACCEPT.V (TENSE? ACCEPT.V)) (THAT _!3)))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(/ (_!1
                         ((! ACKNOWLEDGE.V (TENSE? ACKNOWLEDGE.V))
                          (THAT _!3)))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(/ (_!1
                         ((! ACKNOWLEDGE.V (TENSE? ACKNOWLEDGE.V))
                          (KA _!3)))
                        (_!1 _!3)))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(/ (_!1 ((! ADMIT.V (TENSE? ADMIT.V)) (THAT _!3)))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(/ (_!1 ((! ADMIT.V (TENSE? ADMIT.V)) (KA _!3)))
                        (_!1 _!3)))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                    '(/ (_!1
                         ((! ADMONISH.V (TENSE? ADMONISH.V))
                          (! (+ _!2 (THAT _!3)) (_!2 (THAT _!3)))))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                    '(/ (_!1
                         ((! ADMONISH.V (TENSE? ADMONISH.V))
                          (! (+ _!2 (THAT _!3)) (_!2 (THAT _!3)))))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(/ (_!1 ((! AGREE.V (TENSE? AGREE.V)) (THAT _!3)))
                        (PROBABLY _!3)))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(/ (_!1 ((! AGREE.V (TENSE? AGREE.V)) (KA _!3)))
                        (PROBABLY (_!1 _!3))))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(/ (_!1
                         ((! ALLOW.V (TENSE? ALLOW.V))
                          (! (+ _!2 (KA _!3)) (_!2 (KA _!3)))))
                        (_!2 (CAN.V _!3))))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '- :RULE
                    '(/ (_!1
                         ((! ALLOW.V (TENSE? ALLOW.V))
                          (! (+ _!2 (KA _!3)) (_!2 (KA _!3)))))
                        (NOT (_!2 (CAN.V _!3)))))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                    '(/ ((THAT _!3) ((! AMAZE.V (TENSE? AMAZE.V)) _!1))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                    '(/ ((THAT _!3) ((! AMAZE.V (TENSE? AMAZE.V)) _!1))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                    '(/ (_!1 ((PASV AMAZE.V) (THAT _!3))) _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                    '(/ (_!1 ((PASV AMAZE.V) (THAT _!3))) _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                    '(/ ((THAT _!3) ((! AMUSE.V (TENSE? AMUSE.V)) _!1))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                    '(/ ((THAT _!3) ((! AMUSE.V (TENSE? AMUSE.V)) _!1))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                    '(/ (_!1 ((PASV AMUSE.V) (THAT _!3))) _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                    '(/ (_!1 ((PASV AMUSE.V) (THAT _!3))) _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                    '(/ ((THAT _!3) ((! ANNOY.V (TENSE? ANNOY.V)) _!1))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                    '(/ ((THAT _!3) ((! ANNOY.V (TENSE? ANNOY.V)) _!1))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                    '(/ (_!1 ((PASV ANNOY.V) (THAT _!3))) _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                    '(/ (_!1 ((PASV ANNOY.V) (THAT _!3))) _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                    '(/ (_!1
                         ((! APPRECIATE.V (TENSE? APPRECIATE.V))
                          (THAT _!3)))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                    '(/ (_!1
                         ((! APPRECIATE.V (TENSE? APPRECIATE.V))
                          (THAT _!3)))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                    '(/ ((THAT _!3) (PASV APPRECIATE.V)) _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                    '(/ ((THAT _!3) (PASV APPRECIATE.V)) _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(/ (_!1
                         ((! ASCERTAIN.V (TENSE? ASCERTAIN.V))
                          (THAT _!3)))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(/ ((THAT _!3) (PASV ASCERTAIN.V)) _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                    '(/ ((THAT _!3)
                         ((! ASTONISH.V (TENSE? ASTONISH.V)) _!1))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                    '(/ ((THAT _!3)
                         ((! ASTONISH.V (TENSE? ASTONISH.V)) _!1))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                    '(/ (_!1 ((PASV ASTONISH.V) (THAT _!3))) _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                    '(/ (_!1 ((PASV ASTONISH.V) (THAT _!3))) _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                    '(/ ((THAT _!3)
                         ((! ASTOUND.V (TENSE? ASTOUND.V)) _!1))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                    '(/ ((THAT _!3)
                         ((! ASTOUND.V (TENSE? ASTOUND.V)) _!1))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                    '(/ (_!1 ((PASV ASTOUND.V) (THAT _!3))) _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                    '(/ (_!1 ((PASV ASTOUND.V) (THAT _!3))) _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '- :RULE
                    '(/ (_!1
                         ((! ATTEMPT.V (TENSE? ATTEMPT.V)) (KA _!3)))
                        (NOT (_!1 _!3))))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                    '(/ ((THAT _!3)
                         ((! BAFFLE.V (TENSE? BAFFLE.V)) _!1))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                    '(/ ((THAT _!3)
                         ((! BAFFLE.V (TENSE? BAFFLE.V)) _!1))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                    '(/ (_!1 ((PASV BAFFLE.V) (THAT _!3))) _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                    '(/ (_!1 ((PASV BAFFLE.V) (THAT _!3))) _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(/ (_!1 ((! BEGIN.V (TENSE? BEGIN.V)) (KA _!3)))
                        (_!1 _!3)))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                    '(/ (_!1
                         ((! BERATE.V (TENSE? BERATE.V))
                          (! (+ _!2 (THAT _!3)) (_!2 (THAT _!3)))))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                    '(/ (_!1
                         ((! BERATE.V (TENSE? BERATE.V))
                          (! (+ _!2 (THAT _!3)) (_!2 (THAT _!3)))))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                    '(/ ((THAT _!3)
                         ((! BEWILDER.V (TENSE? BEWILDER.V)) _!1))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                    '(/ ((THAT _!3)
                         ((! BEWILDER.V (TENSE? BEWILDER.V)) _!1))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                    '(/ (_!1 ((PASV BEWILDER.V) (THAT _!3))) _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                    '(/ (_!1 ((PASV BEWILDER.V) (THAT _!3))) _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                    '(/ ((THAT _!3)
                         ((! BOTHER.V (TENSE? BOTHER.V)) _!1))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                    '(/ ((THAT _!3)
                         ((! BOTHER.V (TENSE? BOTHER.V)) _!1))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                    '(/ (_!1 ((PASV BOTHER.V) (THAT _!3))) _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                    '(/ (_!1 ((PASV BOTHER.V) (THAT _!3))) _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(/ (_!1 ((! BOTHER.V (TENSE? BOTHER.V)) (KA _!3)))
                        (_!1 _!3)))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '- :RULE
                    '(/ (_!1 ((! BOTHER.V (TENSE? BOTHER.V)) (KA _!3)))
                        (NOT (_!1 _!3))))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                    '(/ (_!1 ((! CARE.V (TENSE? CARE.V)) (THAT _!3)))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                    '(/ (_!1 ((! CARE.V (TENSE? CARE.V)) (THAT _!3)))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(/ (_!1 ((! CARE.V (TENSE? CARE.V)) (KA _!3)))
                        (_!1 _!3)))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '- :RULE
                    '(/ (_!1 ((! CARE.V (TENSE? CARE.V)) (KA _!3)))
                        (NOT (_!1 _!3))))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(/ (_!1 ((! CEASE.V (TENSE? CEASE.V)) (KA _!3)))
                        (PAST (_!1 _!3))))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '- :RULE
                    '(/ (_!1 ((! CEASE.V (TENSE? CEASE.V)) (KA _!3)))
                        (_!1 _!3)))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(/ (_!1
                         ((! COERCE.V (TENSE? COERCE.V))
                          (! (+ _!2 (KA _!3)) (_!2 (KA _!3)))))
                        (_!2 _!3)))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(/ (_!1 ((! COME.V (TENSE? COME.V)) (KA _!3)))
                        (_!1 _!3)))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(/ (_!1
                         ((! COMPEL.V (TENSE? COMPEL.V))
                          (! (+ _!2 (KA _!3)) (_!2 (KA _!3)))))
                        (_!2 _!3)))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                    '(/ (_!1
                         ((! CONCEDE.V (TENSE? CONCEDE.V)) (THAT _!3)))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                    '(/ ((THAT _!3) (PASV CONCEDE.V)) _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(/ (_!1
                         ((! CONFESS.V (TENSE? CONFESS.V)) (THAT _!3)))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(/ (_!1
                         ((! CONFESS.V (TENSE? CONFESS.V)) (KA _!3)))
                        (_!1 _!3)))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                    '(/ (_!1
                         ((! CONFIRM.V (TENSE? CONFIRM.V)) (THAT _!3)))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                    '(/ ((THAT _!3) (PASV CONFIRM.V)) _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                    '(/ ((THAT _!3)
                         ((! CONFUSE.V (TENSE? CONFUSE.V)) _!1))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                    '(/ ((THAT _!3)
                         ((! CONFUSE.V (TENSE? CONFUSE.V)) _!1))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                    '(/ (_!1 ((PASV CONFUSE.V) (THAT _!3))) _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                    '(/ (_!1 ((PASV CONFUSE.V) (THAT _!3))) _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(/ (_!1
                         ((! CONTINUE.V (TENSE? CONTINUE.V)) (KA _!3)))
                        (_!1 _!3)))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '- :RULE
                    '(/ (_!1
                         ((! CONTINUE.V (TENSE? CONTINUE.V)) (KA _!3)))
                        (PAST (_!1 _!3))))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(/ (_!1
                         ((! CONVINCE.V (TENSE? CONVINCE.V))
                          (! (+ _!2 (KA _!3)) (_!2 (KA _!3)))))
                        (_!2 _!3)))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(/ (_!1 ((! DARE.V (TENSE? DARE.V)) (KA _!3)))
                        (_!1 _!3)))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '- :RULE
                    '(/ (_!1 ((! DARE.V (TENSE? DARE.V)) (KA _!3)))
                        (NOT (_!1 _!3))))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(/ (_!1
                         ((! DECLINE.V (TENSE? DECLINE.V)) (KA _!3)))
                        (NOT (_!1 _!3))))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '- :RULE
                    '(/ (_!1
                         ((! DECLINE.V (TENSE? DECLINE.V)) (KA _!3)))
                        (PROBABLY (_!1 _!3))))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                    '(/ ((THAT _!3)
                         ((! DELIGHT.V (TENSE? DELIGHT.V)) _!1))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                    '(/ ((THAT _!3)
                         ((! DELIGHT.V (TENSE? DELIGHT.V)) _!1))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                    '(/ (_!1 ((PASV DELIGHT.V) (THAT _!3))) _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                    '(/ (_!1 ((PASV DELIGHT.V) (THAT _!3))) _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                    '(/ (_!1
                         ((! DEMONSTRATE.V (TENSE? DEMONSTRATE.V))
                          (THAT _!3)))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                    '(/ ((THAT _!3) (PASV DEMONSTRATE.V)) _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                    '(/ (_!1
                         ((! DEPLORE.V (TENSE? DEPLORE.V)) (THAT _!3)))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                    '(/ (_!1
                         ((! DEPLORE.V (TENSE? DEPLORE.V)) (THAT _!3)))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                    '(/ ((THAT _!3)
                         ((! DEPRESS.V (TENSE? DEPRESS.V)) _!1))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                    '(/ ((THAT _!3)
                         ((! DEPRESS.V (TENSE? DEPRESS.V)) _!1))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                    '(/ (_!1 ((PASV DEPRESS.V) (THAT _!3))) _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                    '(/ (_!1 ((PASV DEPRESS.V) (THAT _!3))) _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                    '(/ (_!1
                         ((! DETEST.V (TENSE? DETEST.V)) (THAT _!3)))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                    '(/ (_!1
                         ((! DETEST.V (TENSE? DETEST.V)) (THAT _!3)))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                    '(/ ((THAT _!3)
                         ((! DISAPPOINT.V (TENSE? DISAPPOINT.V)) _!1))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                    '(/ ((THAT _!3)
                         ((! DISAPPOINT.V (TENSE? DISAPPOINT.V)) _!1))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                    '(/ (_!1 ((PASV DISAPPOINT.V) (THAT _!3))) _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                    '(/ (_!1 ((PASV DISAPPOINT.V) (THAT _!3))) _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                    '(/ ((THAT _!3)
                         ((! DISCONCERT.V (TENSE? DISCONCERT.V)) _!1))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                    '(/ ((THAT _!3)
                         ((! DISCONCERT.V (TENSE? DISCONCERT.V)) _!1))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                    '(/ (_!1 ((PASV DISCONCERT.V) (THAT _!3))) _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                    '(/ (_!1 ((PASV DISCONCERT.V) (THAT _!3))) _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                    '(/ ((THAT _!3)
                         ((! DISCOURAGE.V (TENSE? DISCOURAGE.V)) _!1))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                    '(/ ((THAT _!3)
                         ((! DISCOURAGE.V (TENSE? DISCOURAGE.V)) _!1))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                    '(/ (_!1 ((PASV DISCOURAGE.V) (THAT _!3))) _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                    '(/ (_!1 ((PASV DISCOURAGE.V) (THAT _!3))) _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(/ (_!1
                         ((! DISCOVER.V (TENSE? DISCOVER.V))
                          (THAT _!3)))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                    '(/ ((THAT _!3)
                         ((! DISGUST.V (TENSE? DISGUST.V)) _!1))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                    '(/ ((THAT _!3)
                         ((! DISGUST.V (TENSE? DISGUST.V)) _!1))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                    '(/ (_!1 ((PASV DISGUST.V) (THAT _!3))) _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                    '(/ (_!1 ((PASV DISGUST.V) (THAT _!3))) _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                    '(/ ((THAT _!3)
                         ((! DISILLUSION.V (TENSE? DISILLUSION.V))
                          _!1))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                    '(/ ((THAT _!3)
                         ((! DISILLUSION.V (TENSE? DISILLUSION.V))
                          _!1))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                    '(/ (_!1 ((PASV DISILLUSION.V) (THAT _!3))) _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                    '(/ (_!1 ((PASV DISILLUSION.V) (THAT _!3))) _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                    '(/ (_!1
                         ((! DISLIKE.V (TENSE? DISLIKE.V)) (THAT _!3)))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                    '(/ (_!1
                         ((! DISLIKE.V (TENSE? DISLIKE.V)) (THAT _!3)))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                    '(/ (_!1
                         ((! DISREGARD.V (TENSE? DISREGARD.V))
                          (THAT _!3)))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                    '(/ (_!1
                         ((! DISREGARD.V (TENSE? DISREGARD.V))
                          (THAT _!3)))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                    '(/ ((THAT _!3) (PASV DISREGARD.V)) _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                    '(/ ((THAT _!3) (PASV DISREGARD.V)) _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                    '(/ ((THAT _!3)
                         ((! DISTRESS.V (TENSE? DISTRESS.V)) _!1))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                    '(/ ((THAT _!3)
                         ((! DISTRESS.V (TENSE? DISTRESS.V)) _!1))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                    '(/ (_!1 ((PASV DISTRESS.V) (THAT _!3))) _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                    '(/ (_!1 ((PASV DISTRESS.V) (THAT _!3))) _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                    '(/ ((THAT _!3)
                         ((! EMBARRASS.V (TENSE? EMBARRASS.V)) _!1))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                    '(/ ((THAT _!3)
                         ((! EMBARRASS.V (TENSE? EMBARRASS.V)) _!1))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                    '(/ (_!1 ((PASV EMBARRASS.V) (THAT _!3))) _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                    '(/ (_!1 ((PASV EMBARRASS.V) (THAT _!3))) _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                    '(/ ((THAT _!3)
                         ((! ENCOURAGE.V (TENSE? ENCOURAGE.V)) _!1))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                    '(/ ((THAT _!3)
                         ((! ENCOURAGE.V (TENSE? ENCOURAGE.V)) _!1))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                    '(/ (_!1 ((PASV ENCOURAGE.V) (THAT _!3))) _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                    '(/ (_!1 ((PASV ENCOURAGE.V) (THAT _!3))) _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                    '(/ (_!1 ((! ENVY.V (TENSE? ENVY.V)) (THAT _!3)))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                    '(/ (_!1 ((! ENVY.V (TENSE? ENVY.V)) (THAT _!3)))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                    '(/ ((THAT _!3)
                         ((! EXCITE.V (TENSE? EXCITE.V)) _!1))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                    '(/ ((THAT _!3)
                         ((! EXCITE.V (TENSE? EXCITE.V)) _!1))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                    '(/ (_!1 ((PASV EXCITE.V) (THAT _!3))) _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                    '(/ (_!1 ((PASV EXCITE.V) (THAT _!3))) _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(/ (_!1 ((! FAIL.V (TENSE? FAIL.V)) (KA _!3)))
                        (NOT (_!1 _!3))))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '- :RULE
                    '(/ (_!1 ((! FAIL.V (TENSE? FAIL.V)) (KA _!3)))
                        (_!1 _!3)))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(/ (_!1 ((! FIND.V (TENSE? FIND.V)) (THAT _!3)))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(/ (_!1
                         ((! FIND.V (TENSE? FIND.V))
                          (! (+ _!2 (KA _!3)) (_!2 (KA _!3)))))
                        (PROBABLY (_!2 _!3))))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(/ (_!1
                         ((! FORCE.V (TENSE? FORCE.V))
                          (! (+ _!2 (KA _!3)) (_!2 (KA _!3)))))
                        (_!2 _!3)))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(/ (_!1 ((PASV FORCE.V) (KA _!3))) (_!1 _!3)))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                    '(/ (_!1
                         ((! FORGET.V (TENSE? FORGET.V)) (THAT _!3)))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                    '(/ (_!1
                         ((! FORGET.V (TENSE? FORGET.V)) (THAT _!3)))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(/ (_!1 ((! FORGET.V (TENSE? FORGET.V)) (KA _!3)))
                        (NOT (_!1 _!3))))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '- :RULE
                    '(/ (_!1 ((! FORGET.V (TENSE? FORGET.V)) (KA _!3)))
                        (_!1 _!3)))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(/ (_!1 ((! GET.V (TENSE? GET.V)) (KA _!3)))
                        (_!1 _!3)))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '- :RULE
                    '(/ (_!1 ((! GET.V (TENSE? GET.V)) (KA _!3)))
                        (NOT (_!1 _!3))))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(/ (_!1 ((! HAPPEN.V (TENSE? HAPPEN.V)) (KA _!3)))
                        (_!1 _!3)))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                    '(/ (_!1 ((! HATE.V (TENSE? HATE.V)) (KA _!3)))
                        (_!1 _!3)))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                    '(/ (_!1 ((! HATE.V (TENSE? HATE.V)) (KA _!3)))
                        (_!1 _!3)))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(/ (_!1 ((! HASTEN.V (TENSE? HASTEN.V)) (KA _!3)))
                        (_!1 _!3)))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(/ (_!1
                         ((! HELP.V (TENSE? HELP.V))
                          (! (+ _!2 (KA _!3)) (_!2 (KA _!3)))))
                        (_!2 _!3)))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '- :RULE
                    '(/ (_!1
                         ((! HESITATE.V (TENSE? HESITATE.V)) (KA _!3)))
                        (PROBABLY (_!1 _!3))))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                    '(/ ((THAT _!3)
                         ((! HORRIFY.V (TENSE? HORRIFY.V)) _!1))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                    '(/ ((THAT _!3)
                         ((! HORRIFY.V (TENSE? HORRIFY.V)) _!1))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                    '(/ (_!1 ((PASV HORRIFY.V) (THAT _!3))) _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                    '(/ (_!1 ((PASV HORRIFY.V) (THAT _!3))) _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                    '(/ (_!1
                         ((! IGNORE.V (TENSE? IGNORE.V)) (THAT _!3)))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                    '(/ (_!1
                         ((! IGNORE.V (TENSE? IGNORE.V)) (THAT _!3)))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                    '(/ ((THAT _!3) (PASV IGNORE.V)) _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                    '(/ ((THAT _!3) (PASV IGNORE.V)) _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                    '(/ ((THAT _!3)
                         ((! IMPRESS.V (TENSE? IMPRESS.V)) _!1))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                    '(/ ((THAT _!3)
                         ((! IMPRESS.V (TENSE? IMPRESS.V)) _!1))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                    '(/ (_!1 ((PASV IMPRESS.V) (THAT _!3))) _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                    '(/ (_!1 ((PASV IMPRESS.V) (THAT _!3))) _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                    '(/ (_!1 ((! KNOW.V (TENSE? KNOW.V)) (THAT _!3)))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                    '(/ (_!1 ((! KNOW.V (TENSE? KNOW.V)) (THAT _!3)))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                    '(/ ((THAT _!3) (PASV KNOW.V)) _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                    '(/ ((THAT _!3) (PASV KNOW.V)) _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                    '(/ (_!1
                         ((! LAMENT.V (TENSE? LAMENT.V)) (THAT _!3)))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                    '(/ (_!1
                         ((! LAMENT.V (TENSE? LAMENT.V)) (THAT _!3)))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                    '(/ (_!1 ((! LEARN.V (TENSE? LEARN.V)) (THAT _!3)))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                    '(/ (_!1 ((! LEARN.V (TENSE? LEARN.V)) (THAT _!3)))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                    '(/ (_!1 ((! LIE.V (TENSE? LIE.V)) (THAT _!3)))
                        (NOT _!3)))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                    '(/ (_!1 ((! LIE.V (TENSE? LIE.V)) (THAT _!3)))
                        (NOT _!3)))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(/ (_!1 ((! LIKE.V (TENSE? LIKE.V)) (KA _!3)))
                        (_!1 _!3)))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(/ (_!1 ((! LOVE.V (TENSE? LOVE.V)) (KA _!3)))
                        (_!1 _!3)))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(/ (_!1 ((! MANAGE.V (TENSE? MANAGE.V)) (KA _!3)))
                        (_!1 _!3)))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '- :RULE
                    '(/ (_!1 ((! MANAGE.V (TENSE? MANAGE.V)) (KA _!3)))
                        (NOT (_!1 _!3))))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '- :RULE
                    '(/ (_!1 ((! MEAN.V (TENSE? MEAN.V)) (KA _!3))) (_!1 _!3)))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                    '(/ (_!1
                         ((! MENTION.V (TENSE? MENTION.V)) (THAT _!3)))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                    '(/ ((THAT _!3) (PASV MENTION.V)) _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                    '(/ (_!1 ((! MISS.V (TENSE? MISS.V)) (KA _!3)))
                        (PAST (_!1 _!3))))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                    '(/ (_!1 ((! MISS.V (TENSE? MISS.V)) (KA _!3)))
                        (PAST (_!1 _!3))))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                    '(/ (_!1
                         ((! NEGLECT.V (TENSE? NEGLECT.V)) (THAT _!3)))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                    '(/ (_!1
                         ((! NEGLECT.V (TENSE? NEGLECT.V)) (THAT _!3)))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                    '(/ ((THAT _!3) (PASV NEGLECT.V)) _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                    '(/ ((THAT _!3) (PASV NEGLECT.V)) _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(/ (_!1
                         ((! NEGLECT.V (TENSE? NEGLECT.V)) (KA _!3)))
                        (NOT (_!1 _!3))))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '- :RULE
                    '(/ (_!1
                         ((! NEGLECT.V (TENSE? NEGLECT.V)) (KA _!3)))
                        (_!1 _!3)))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                    '(/ (_!1 ((! NOTE.V (TENSE? NOTE.V)) (THAT _!3)))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                    '(/ (_!1 ((! NOTE.V (TENSE? NOTE.V)) (THAT _!3)))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                    '(/ (_!1
                         ((! NOTICE.V (TENSE? NOTICE.V)) (THAT _!3)))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                    '(/ (_!1
                         ((! NOTICE.V (TENSE? NOTICE.V)) (THAT _!3)))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(/ (_!1
                         ((! OBSERVE.V (TENSE? OBSERVE.V)) (THAT _!3)))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                    '(/ ((THAT _!3)
                         ((! OUTRAGE.V (TENSE? OUTRAGE.V)) _!1))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                    '(/ ((THAT _!3)
                         ((! OUTRAGE.V (TENSE? OUTRAGE.V)) _!1))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                    '(/ (_!1 ((PASV OUTRAGE.V) (THAT _!3))) _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                    '(/ (_!1 ((PASV OUTRAGE.V) (THAT _!3))) _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                    '(/ (_!1
                         ((! OVERLOOK.V (TENSE? OVERLOOK.V))
                          (THAT _!3)))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                    '(/ (_!1
                         ((! OVERLOOK.V (TENSE? OVERLOOK.V))
                          (THAT _!3)))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                    '(/ ((THAT _!3) (PASV OVERLOOK.V)) _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                    '(/ ((THAT _!3) (PASV OVERLOOK.V)) _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(/ (_!1
                         ((! PERMIT.V (TENSE? PERMIT.V))
                          (! (+ _!2 (KA _!3)) (_!2 (KA _!3)))))
                        (_!2 (CAN.V _!3))))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '- :RULE
                    '(/ (_!1
                         ((! PERMIT.V (TENSE? PERMIT.V))
                          (! (+ _!2 (KA _!3)) (_!2 (KA _!3)))))
                        (NOT (_!2 (CAN.V _!3)))))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(/ (_!1
                         ((! PERSUADE.V (TENSE? PERSUADE.V))
                          (! (+ _!2 (KA _!3)) (_!2 (KA _!3)))))
                        (_!2 _!3)))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                    '(/ (_!1 ((! PITY.V (TENSE? PITY.V)) (THAT _!3)))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                    '(/ (_!1 ((! PITY.V (TENSE? PITY.V)) (THAT _!3)))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                    '(/ ((THAT _!3) (PASV PITY.V)) _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                    '(/ ((THAT _!3) (PASV PITY.V)) _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                    '(/ (_!1 ((PASV PLEASE.V) (THAT _!3))) _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                    '(/ (_!1 ((PASV PLEASE.V) (THAT _!3))) _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                    '(/ ((THAT _!3)
                         ((! PLEASE.V (TENSE? PLEASE.V)) _!1))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                    '(/ ((THAT _!3)
                         ((! PLEASE.V (TENSE? PLEASE.V)) _!1))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                    '(/ (_!1
                         ((! PRAISE.V (TENSE? PRAISE.V)) (THAT _!3)))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                    '(/ (_!1
                         ((! PRAISE.V (TENSE? PRAISE.V)) (THAT _!3)))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                    '(/ ((THAT _!3) (PASV PRAISE.V)) _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                    '(/ ((THAT _!3) (PASV PRAISE.V)) _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                    '(/ (_!1
                         ((! PRETEND.V (TENSE? PRETEND.V)) (THAT _!3)))
                        (NOT _!3)))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                    '(/ (_!1
                         ((! PRETEND.V (TENSE? PRETEND.V)) (THAT _!3)))
                        (PROBABLY (NOT _!3))))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                    '(/ (_!1
                         ((! PRETEND.V (TENSE? PRETEND.V)) (KA _!3)))
                        (NOT (_!1 _!3))))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                    '(/ (_!1
                         ((! PRETEND.V (TENSE? PRETEND.V)) (KA _!3)))
                        (PROBABLY (NOT (_!1 _!3)))))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(/ (_!1
                         ((! PROHIBIT.V (TENSE? PROHIBIT.V))
                          (! (+ _!2 (KA _!3)) (_!2 (KA _!3)))))
                        (NOT (_!2 _!3))))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(/ (_!1 ((! PROVE.V (TENSE? PROVE.V)) (THAT _!3)))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(/ ((THAT _!3) (PASV PROVE.V)) _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(/ (_!1 ((! PROVE.V (TENSE? PROVE.V)) _!2))
                        (_!1 IS-A _!2)))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                    '(/ (_!1
                         ((! REALIZE.V (TENSE? REALIZE.V)) (THAT _!3)))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                    '(/ (_!1
                         ((! REALIZE.V (TENSE? REALIZE.V)) (THAT _!3)))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(/ (_!1
                         ((! RECALL.V (TENSE? RECALL.V)) (THAT _!3)))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(/ (_!1
                         ((! RECOLLECT.V (TENSE? RECOLLECT.V))
                          (THAT _!3)))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                    '(/ (_!1
                         ((! RECOGNIZE.V (TENSE? RECOGNIZE.V))
                          (THAT _!3)))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                    '(/ (_!1
                         ((! RECOGNIZE.V (TENSE? RECOGNIZE.V))
                          (THAT _!3)))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(/ (_!1
                         ((! REFLECT.V (TENSE? REFLECT.V)) (THAT _!3)))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(/ (_!1
                         ((! REFRAIN-FROM.V (TENSE? REFRAIN-FROM.V))
                          (KA _!3)))
                        (NOT (_!1 _!3))))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '- :RULE
                    '(/ (_!1
                         ((! REFRAIN-FROM.V (TENSE? REFRAIN-FROM.V))
                          (KA _!3)))
                        (_!1 _!3)))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(/ (_!1 ((! REFUSE.V (TENSE? REFUSE.V)) (KA _!3)))
                        (NOT (_!1 _!3))))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '- :RULE
                    '(/ (_!1 ((! REFUSE.V (TENSE? REFUSE.V)) (KA _!3)))
                        (PROBABLY (_!1 _!3))))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                    '(/ (_!1
                         ((! REGRET.V (TENSE? REGRET.V)) (THAT _!3)))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                    '(/ (_!1
                         ((! REGRET.V (TENSE? REGRET.V)) (THAT _!3)))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                    '(/ (_!1 ((! REGRET.V (TENSE? REGRET.V)) (KA _!3)))
                        (_!1 _!3)))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                    '(/ (_!1 ((! REGRET.V (TENSE? REGRET.V)) (KA _!3)))
                        (_!1 _!3)))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                    '(/ ((THAT _!3) (PASV REGRET.V)) _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                    '(/ ((THAT _!3) (PASV REGRET.V)) _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                    '(/ (_!1
                         ((! REJOICE.V (TENSE? REJOICE.V)) (THAT _!3)))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                    '(/ (_!1
                         ((! REJOICE.V (TENSE? REJOICE.V)) (THAT _!3)))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(/ (_!1
                         ((! REMEMBER.V (TENSE? REMEMBER.V))
                          (THAT _!3)))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(/ (_!1
                         ((! REMEMBER.V (TENSE? REMEMBER.V)) (KA _!3)))
                        (_!1 _!3)))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '- :RULE
                    '(/ (_!1
                         ((! REMEMBER.V (TENSE? REMEMBER.V)) (KA _!3)))
                        (NOT (_!1 _!3))))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(/ (_!1
                         ((! REMIND.V (TENSE? REMIND.V))
                          (! (+ _!2 (KA _!3)) (_!2 (KA _!3)))))
                        (PROBABLY (_!2 _!3))))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(/ (_!1
                         ((! RESTRAIN-FROM.V (TENSE? RESTRAIN-FROM.V))
                          (KA _!3)))
                        (NOT (_!1 _!3))))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '- :RULE
                    '(/ (_!1
                         ((! RESTRAIN-FROM.V (TENSE? RESTRAIN-FROM.V))
                          (KA _!3)))
                        (_!1 _!3)))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(/ (_!1 ((! RESUME.V (TENSE? RESUME.V)) (KA _!3)))
                        (_!1 _!3)))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '- :RULE
                    '(/ (_!1 ((! RESUME.V (TENSE? RESUME.V)) (KA _!3)))
                        (PAST (_!1 _!3))))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                    '(/ (_!1
                         ((! REVEAL.V (TENSE? REVEAL.V)) (THAT _!3)))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                    '(/ (_!1
                         ((! REVEAL.V (TENSE? REVEAL.V)) (THAT _!3)))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                    '(/ ((THAT _!3) (PASV REVEAL.V)) _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                    '(/ ((THAT _!3) (PASV REVEAL.V)) _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                    '(/ ((THAT _!3) ((! SCARE.V (TENSE? SCARE.V)) _!1))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                    '(/ ((THAT _!3) ((! SCARE.V (TENSE? SCARE.V)) _!1))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                    '(/ (_!1 ((PASV SCARE.V) (THAT _!3))) _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                    '(/ (_!1 ((PASV SCARE.V) (THAT _!3))) _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                    '(/ (_!1
                         ((! SCOLD.V (TENSE? SCOLD.V))
                          (! (+ _!2 (THAT _!3)) (_!2 (THAT _!3)))))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                    '(/ (_!1
                         ((! SCOLD.V (TENSE? SCOLD.V))
                          (! (+ _!2 (THAT _!3)) (_!2 (THAT _!3)))))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                    '(/ (_!1 ((! SEE.V (TENSE? SEE.V)) (THAT _!3)))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                    '(/ ((THAT _!3) ((! SHOCK.V (TENSE? SHOCK.V)) _!1))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                    '(/ ((THAT _!3) ((! SHOCK.V (TENSE? SHOCK.V)) _!1))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                    '(/ (_!1 ((PASV SHOCK.V) (THAT _!3))) _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                    '(/ (_!1 ((PASV SHOCK.V) (THAT _!3))) _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(/ (_!1 ((! SHOW.V (TENSE? SHOW.V)) (THAT _!3)))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(/ (_!1 ((! START.V (TENSE? START.V)) (KA _!3)))
                        (_!1 _!3)))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(/ (_!1 ((! STOP.V (TENSE? STOP.V)) (KA _!3)))
                        (PAST (_!1 _!3))))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '- :RULE
                    '(/ (_!1 ((! STOP.V (TENSE? STOP.V)) (KA _!3)))
                        (_!1 _!3)))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(/ (_!1
                         ((! SUCCEED-IN.V (TENSE? SUCCEED-IN.V))
                          (KA _!3)))
                        (_!1 _!3)))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '- :RULE
                    '(/ (_!1
                         ((! SUCCEED-IN.V (TENSE? SUCCEED-IN.V))
                          (KA _!3)))
                        (NOT (_!1 _!3))))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                    '(/ ((THAT _!3)
                         ((! SURPRISE.V (TENSE? SURPRISE.V)) _!1))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                    '(/ ((THAT _!3)
                         ((! SURPRISE.V (TENSE? SURPRISE.V)) _!1))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                    '(/ (_!1 ((PASV SURPRISE.V) (THAT _!3))) _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                    '(/ (_!1 ((PASV SURPRISE.V) (THAT _!3))) _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '- :RULE
                    '(/ (_!1
                         ((! SUSPECT.V (TENSE? SUSPECT.V)) (THAT _!3)))
                        (PROBABLY _!3)))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(/ (_!1 ((! TEND.V (TENSE? TEND.V)) (KA _!3)))
                        (_!1 _!3)))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                    '(/ (_!1 ((PASV TOUCH.V) (THAT _!3))) _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                    '(/ (_!1 ((PASV TOUCH.V) (THAT _!3))) _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                    '(/ ((THAT _!3) ((! TOUCH.V (TENSE? TOUCH.V)) _!1))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                    '(/ ((THAT _!3) ((! TOUCH.V (TENSE? TOUCH.V)) _!1))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                    '(/ (_!1 ((PASV TROUBLE.V) (THAT _!3))) _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                    '(/ (_!1 ((PASV TROUBLE.V) (THAT _!3))) _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                    '(/ ((THAT _!3)
                         ((! TROUBLE.V (TENSE? TROUBLE.V)) _!1))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                    '(/ ((THAT _!3)
                         ((! TROUBLE.V (TENSE? TROUBLE.V)) _!1))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(/ (_!1 ((! TURN-OUT.V (TENSE? TURN-OUT.V)) _!2))
                        (_!1 IS-A _!2)))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '- :RULE
                    '(/ (_!1 ((! TURN-OUT.V (TENSE? TURN-OUT.V)) _!2))
                        (NOT (_!1 IS-A _!2))))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                    '(/ (_!1
                         ((! UNDERSTAND.V (TENSE? UNDERSTAND.V))
                          (THAT _!3)))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                    '(/ (_!1
                         ((! UNDERSTAND.V (TENSE? UNDERSTAND.V))
                          (THAT _!3)))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                    '(/ ((THAT _!3) (PASV UNDERSTAND.V)) _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                    '(/ ((THAT _!3) (PASV UNDERSTAND.V)) _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                    '(/ ((THAT _!3)
                         ((! UNNERVE.V (TENSE? UNNERVE.V)) _!1))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                    '(/ ((THAT _!3)
                         ((! UNNERVE.V (TENSE? UNNERVE.V)) _!1))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                    '(/ (_!1 ((PASV UNNERVE.V) (THAT _!3))) _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                    '(/ (_!1 ((PASV UNNERVE.V) (THAT _!3))) _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(/ (_!1
                         ((! USE.V (TENSE? USE.V))
                          (! (+ _!2 (KA _!3)) (_!2 (KA _!3)))))
                        (_!1 _!3)))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(/ (_!1
                         ((! USED-TO.V (TENSE? USED-TO.V)) (KA _!3)))
                        (PAST (_!1 _!3))))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(/ (_!1
                         ((! VERIFY.V (TENSE? VERIFY.V)) (THAT _!3)))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                    '(/ ((THAT _!3) (PASV VERIFY.V)) _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '- :RULE
                    '(/ (_!1 ((! WARN.V (TENSE? WARN.V)) (THAT _!3)))
                        _!3))
     (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '- :RULE
                    '(/ ((THAT _!3) (PASV WARN.V)) _!3))))


(defun find-imp-verb-in (rule)
    (cond
        ((null rule) nil)
        ((and (atom rule) (search ".V" (string rule))) rule)
        ((atom rule) nil)
        (t (dolist (s rule)
            (setq res (find-imp-verb-in s))
            (if res (return-from find-imp-verb-in res))))))

(defparameter *IMPLICATIVES*
    (remove-duplicates (mapcar (lambda (x) (find-imp-verb-in (slot-value x 'rule))) *INFER-FROM-IMPLICATIVE-RULES*)))