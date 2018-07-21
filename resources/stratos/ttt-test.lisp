(load "../../ttt/src/load")

;; F1
(defparameter *accept*
  ;; (s '(all_wff w
  ;;  (all x ((x accept.v (that w)) => w))))
  '(/
    (_! ((! accept.v (tense? accept.v)) (that _!2)))
    _!2))

;; F1
(defparameter *know*
  ;; (store-prs-ir '(((w_wff) (x)) (x know.v (that w)) w))
  '(/
    (_! ((! know.v (tense? know.v)) (that _!2)))
    _!2))

;; F2
(defparameter *confess*
  ;; (s '(all_pred p 
  ;;  (all x ((x confess.v (ka p)) => (x p)))))
  '(/
    (_! ((! confess.v (tense? confess.v)) (ka _!2)))
    (_! _!2)))

(defparameter *allow*
  ;; (s '(all_pred p
  ;;  (all x (all y ((x allow.v (ka p) y) => (y (can.v p)))))))
  '(/
    (_! ((! allow.v (tense? allow.v)) (! (+ _!1 (ka _!2)) (_!1 (ka _!2)))))
    (_!1 (can.v _!2))))

(defparameter *turn-out*
  ;; (s '(all x (all y ((x turn-out.v y) => (x is-a y)))))
  '(/
    (_! ((! turn-out.v (tense? turn-out.v)) _!2))
    (_! is-a _!2)))

(defparameter *ascertain*
  ;; (s '(all_wff w (((that w) (pasv ascertain.v)) => w)))
  '(/
    ((that _!2) (pasv ascertain.v))
    _!2))

(defparameter *agree*
  '(/
    (_!1 ((! AGREE.V (TENSE? AGREE.V)) (THAT _!3)))
    (PROBABLY _!3)))

(defparameter *lie*
  '(/
    (_!1 ((! LIE.V (TENSE? LIE.V)) (THAT _!3)))
    (NOT _!3)))

(defparameter *shock*
  '(/
    ((THAT _!3) ((! SHOCK.V (TENSE? SHOCK.V)) _!1))
    _!3))

(defparameter *scold*
 '(/
    (_!1 ((! SCOLD.V (TENSE? SCOLD.V)) (! (+ _!2 (THAT _!3)) (_!2 (THAT _!3)))))
    _!3))

(defparameter *continue*
  '(/
    (_!1 ((! CONTINUE.V (TENSE? CONTINUE.V)) (KA _!3)))
    (PAST (_!1 _!3))))

(defparameter *date*
  '(/
    (_!1 ((! DARE.V (TENSE? DARE.V)) (KA _!3)))
    (NOT (_!1 _!3))))

(defparameter *force*
  '(/
    (_!1 ((PASV FORCE.V) (KA _!3)))
    (_!1 _!3)))

(defparameter *mean*
  '(/
    (_!1 ((PAST MEAN.V) (KA _!3)))
    (_!1 _!3)))

(defparameter *use*
'(/
  (_!1 ((! USE.V (TENSE? USE.V)) (! (+ _!2 (KA _!3)) (_!2 (KA _!3)))))
  (_!1 _!3)))


(defun tense? (l)
  (if (member l '(PAST PRES PERF PROG)) t nil))

;; (defun verb? (ex)
;;   (cond ((and (symbolp ex) (search ".V" (string ex))) t)
;;   ((and (listp ex) (= 2 (length ex)) (member (first ex) '(past pres pasv prog)) (symbolp (second ex)) (search ".V" (string (second ex)))) t)
;;   (t nil)))


(print (ttt:apply-rule *accept* '(|Jack| (accept.v (that (|Mary| (was.v cold.a))))) :shallow t))
(print (ttt:apply-rule *know* '((the.d man.n) ((past know.v) (that (|Mary| (was.v cold.a))))) :shallow t))
(print (ttt:apply-rule *confess* '(|John| ((past confess.v) (ka steal.v))) :shallow t))
(print (ttt:apply-rule *allow* '(|John| ((past allow.v) |Mary| (ka swim.v))) :shallow t))
(print (ttt:apply-rule *allow* '(|John| ((past allow.v) ((the.d man.n) (ka swim.v)))) :shallow t))
(print (ttt:apply-rule *turn-out* '((the.d program.n) ((past turn-out.v) (ka (be.v (a.d fraud.n))))) :shallow t))
(print (ttt:apply-rule *ascertain* '((that (|John| ((past be.v) not drunk.a))) (pasv ascertain.v)) :shallow t))
(print (ttt:apply-rule *agree* '(|John| ((past agree.v) (that (he.pro ((past kill.v) |Tom|))))) :shallow t))
(print (ttt:apply-rule *lie* '(|John| ((past lie.v) (that (he.pro ((past be.v) over.p eighteen.n))))) :shallow t))
(print (ttt:apply-rule *shock* '((that ((his.d friend.n) ((past betray.v) him.pro))) ((past shock.v) |John|)) :shallow t))
(print (ttt:apply-rule *scold* '(|John| ((past scold.v) |Mary| (that (she.pro ((past leave.v) (a.d mess.n)))))) :shallow t))
(print (ttt:apply-rule *scold* '(|John| ((past scold.v) (|Mary| (that (she.pro ((past leave.v) (a.d mess.n))))))) :shallow t))
(print (ttt:apply-rule *continue* '(|John| ((pres continue.v) (ka (take.v (piano.a lesson.n))))) :shallow t))
(print (ttt:apply-rule *force* '(|John| ((pasv force.v) (ka (clean.v (the.d house.n))))) :shallow t))
(print (ttt:apply-rule *mean* '(|John| ((past mean.v) (ka (harm.v |Mary|)))) :shallow t))
(print (ttt:apply-rule *use* '(|John| ((past use.v) |Lisp| (ka (write.v |Epilog|)))) :shallow t))
(print (ttt:apply-rule *use* '(|John| ((past use.v) (|Lisp| (ka (write.v |Epilog|))))) :shallow t))

;; (load "ttt-test.lisp")