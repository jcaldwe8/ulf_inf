;; Gene Kim 7-27-2018
;; 
;; Code for the core inference classes and mechanisms, which need to be
;; available to the rest of the system.

;; TODO: remove these once we load in the actual macro version.
(defun trivial-normalization (ulf) ulf)
(defun normalize-aliases (ulf) ulf)
(defun expand-macros (ulf) ulf)
(defun fully-normalize (ulf) ulf)
;; TODO: remove these once we load in real inference functions from
;; elsewhere.
(defun premacro-question-inferences (ulf) nil)
(defun premacro-implicative-inferences (ulf) nil)
(defun it-cleft-inferences (ulf) nil)
(defun natural-logic-entailments (ulfs) nil) 
(defun implicative-entailments (ulf) nil)


;; Define class for inference results.
(defclass inf-result ()
  ((result-formula
     :initarg :result-formula
     :initform (error "Must supply a result formula.")
     :accessor result-formula)
   (inf-scopes ; Scopes at which the inference is valid.
     :initarg :inf-scopes
     :initform '(local)
     :accessor inf-scopes)
   (is-entailment ; whether this inference result is an entailment.
     :initarg :is-entailment
     :initform nil
     :accessor is-entailment) ; default to non-entailment.
   (inf-rule
     :initarg :inf-rule
     :initform nil
     :accessor inf-rule)
   (polarity-context
     :initarg :polarity-context
     :initform nil
     :accessor polarity-context)
   (src-local-ulf
     :initarg :src-local-ulf
     :initform (error "Must supply a src-local-ulf")
     :accessor src-local-ulf)
   (src-parent-ulf
     :initarg :src-parent-ulf
     :initform (error "Must supply a src-parent-ulf")
     :accessor src-parent-ulf)
   (src-full-ulf
     :initarg :src-full-ulf
     :initform (error "Must supply a src-full-ulf")
     :accessor src-full-ulf)))

;; This is a version of 'all-rule-result' from util-from-pilot-project.lisp
;; that returns a list of inf-result classes rather than simply the inferred
;; formulas.
(defun all-ttt-rule-inf-result (tttrule roottree)
  (labels
    (;; Recursive helper function.
     ;; Does all the heavy lifting.
     (rec-helper (rule tree parent root)
       (let (result infered? infres recurd)
         (setq result (ttt:apply-rule rule tree :shallow t :max-n 1))
         (setq infered? (and result (not (equal result tree))))
         (if infered? 
           ;; Construct inference result if one was made.
           (setq infres
                 (make-instance 
                   'inf-result
                   :result-formula result
                   :inf-rule rule
                   :src-local-ulf tree
                   :src-parent-ulf parent
                   :src-full-ulf root)))
         (cond
           ;; [Base cases]
           ;; 1. Made inference, leaf node.
           ;; -> Just construct inf-result and return.
           ((and infered? (atom tree))
            (list infres))
           ;; 2. No inference, leaf node.
           ;; -> Return nil
           ((atom tree) nil)
           ;; [Recursive cases]
           (t 
             (setq recurd
                   (append (rec-helper rule (car tree) tree root)
                           (rec-helper rule (cdr tree) tree root)))
             (if infered?
               ;; 3. Made inference, non-leaf node.
               ;; -> Add inference to recursed result.
               (cons infres recurd)
               ;; 4. No inference, non-leaf node.
               ;; -> Return just the recursed result.
               recurd)))))
     ) ; end of labels definitions.

    ;; Main body.
    (rec-helper tttrule roottree nil roottree)))
                           


