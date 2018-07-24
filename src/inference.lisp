;;; Gene Kim 7-23-2018
;;;
;;; Code for high-level inference management.

;; TODO: remove these once we load in the actual macro version.
(defun trivial-normalization (ulf) ulf)
(defun normalize-aliases (ulf) ulf)
(defun expand-macros (ulf) ulf)
(defun fully-normalize (ulf) ulf)
;; TODO: remove these once we load in real inference functions from
;; elsewhere.
(defun premacro-request-inferences (ulf) nil)
(defun premacro-counterfactual-inferences (ulf) nil)
(defun premacro-question-inferences (ulf) nil)
(defun premacro-implicative-inferences (ulf) nil)
(defun it-cleft-inference (ulf) nil)

;; Define class for inference results.
(defclass inf-result ()
  ((result-formula
     :initarg :result-formula
     :initform (error "Must supply a result formula."))
   (inf-scopes ; Scopes at which the inference is valid.
     :initarg :inf-scopes
     :initform '(local))
   (is-entailment ; whether this inference result is an entailment.
     :initarg :is-entailment
     :initform nil) ; default to non-entailment.
   (inf-rule
     :initarg :inf-rule
     :initform nil)
   (polarity-context
     :initarg :inf-rule 
     :initform '+) ; default to positive polarity
   (src-local-ulf
     :initarg :src-local-ulf
     :initform (error "Must supply a src-local-ulf"))
   (src-parent-ulf
     :initarg :src-parent-ulf
     :initform (error "Must supply a src-parent-ulf"))
   (src-full-ulf
     :initarg :src-full-ulf
     :initform (error "Must supply a src-full-ulf"))))


;; Returns t if inf-result allows recursing inferences from this result.
;; Raises an error if the input is not an inf-result class.
(defun recursable-inference? (inf-result)
  (if (typep inf-result 'inf-result)
    (slot-value inf-result 'is-entailment)
    (error "ERROR: The argument to recursable-inference? must be an instance of class inf-result.")))


;; Functions for initial ULF normalization.
(defparameter *initial-ulf-normalization-fns*
  (list #'normalize-aliases
        #'trivial-normalization))
;; Performs initial ULF normalization, NOT INCLUDING
;;  1. macro expansions or
;;  2. word reordering.
(defun initial-ulf-normalization (ulf)
  (reduce #'(lambda (acc new) (funcall acc new)) 
          *initial-ulf-normalization-fns* 
          :initial-value ulf))

;; Functions that perform inferences from premacro ULFs.
(defparameter *premacro-inference-fns*
  (list #'premacro-request-inferences
        #'premacro-counterfactual-inferences
        #'premacro-question-inferences
        #'premacro-implicative-inferences
        #'it-cleft-inference))
;; Obtains inferences from ULFs before macros are applied.
;; These include inferences that rely on the question inversions or cleft
;; constructions.
(defun premacro-inferences (ulf)
  (apply #'concatenate 'string
         (mapcar #'(lambda (inf-fn) (funcall inf-fn ulf))
                 *premacro-inference-fns*)))

;; Performs all possible inferences on the given ulf.
;; Returns a list of inferences.
(defun infer-all (ulf)
  (let ((ulf1 (initial-ulf-normalization ulf))
        ulf1m ulf2 preinfs postinfs recableinfs recinfs)
    ;; Perform preliminary inferences.
    (setq preinfs (premacro-inferences ulf1))
    ;; Expand macros and fully normalize.
    (setq ulf1m (expand-macros ulf1))
    (setq ulf2 (fully-normalize ulf1m))
    ;; Perform post-macro inferences.
    (setq postinfs (postmacro-inferences ulf2))
    ;; Recursively infer where applicable.
    (setq recableinfs (remove-if-not #'recursable-inference?
                                     (concatenate 'list preinfs postinfs)))
    (setq recinfs (concatenate 'list
                               (mapcar #'infer-all recableinfs)))
    ;; Return all inferences in a list.
    (concatenate 'list preinfs postinfs recinfs)))

