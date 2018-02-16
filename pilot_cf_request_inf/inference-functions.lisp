;; Nov 4/17
;; SEE ./README FOR THE PURPOSE OF THIS CODE.
;; THIS FILE DEPENDS ON "inference-rules-and-helpers.lisp", AND TTT
;; =========================================================================

; (load "/p/nl/tools/ttt/src/load")  ; moved to "init.lisp"

; Store the names of the (functionalized) rules below; the starred
; versions (e.g., *infer-want-from-request*) are TTT rules as defined
; in ./inference-rules-and-helpers:

(defparameter *rule-names* 
 '(infer-want-from-request infer-expect-from-request 
   infer-falsehood-from-positive-counterfactual 
   infer-falsehood-from-inverted-positive-counterfactual
   infer-fact-from-negative-counterfactual
   infer-fact-from-inverted-negative-counterfactual
 ))

; Now the functionalized rules themselves (applying the TTT rules at
; multiple depths):

(defun infer-want-from-request (ulf)
;``````````````````````````````````
 (leftmost-rule-result *infer-want-from-request* ulf))


(defun infer-expect-from-request (ulf)
;`````````````````````````````````````
 (leftmost-rule-result *infer-expect-from-request* ulf))


(defun infer-falsehood-from-positive-counterfactual (ulf)
;```````````````````````````````````````````````````````
 (leftmost-rule-result *infer-falsehood-from-positive-counterfactual* ulf))


(defun infer-falsehood-from-inverted-positive-counterfactual (ulf)
;````````````````````````````````````````````````````````````````
 (leftmost-rule-result *infer-falsehood-from-inverted-positive-counterfactual* ulf))


(defun infer-fact-from-negative-counterfactual (ulf)
;```````````````````````````````````````````````````
 (leftmost-rule-result *infer-fact-from-negative-counterfactual* ulf))


(defun infer-fact-from-inverted-negative-counterfactual (ulf)
;```````````````````````````````````````````````````````````
 (leftmost-rule-result *infer-fact-from-inverted-negative-counterfactual* ulf))


(defun leftmost-rule-result (rule tree)
;`````````````````````````````````````
; TTT operator ^* doesn't always work, so this is intended for finding 
; a subtree of 'tree' to which 'rule' applies (yielding something different
; from 'tree' itself and from nil), and returning the result
 (let (result)
      (setq result (ttt:apply-rule rule tree :shallow t :max-n 1))
      (cond ((and result (not (equal result tree))) result)
            ((and tree (listp tree))
             (setq result (leftmost-rule-result rule (car tree)))
             (if (and result (not (equal result (car tree))))
                 result (leftmost-rule-result rule (cdr tree))))
            (t tree)); **hmm, will tree necessarily be nil here? (OK if so)
 )); end of leftmost-rule-result


(defun results-from-applying-rules (rule-names ulfs filter-out-failures)
;``````````````````````````````````````````````````````````````````````
; To apply a list of rules to a list of ulfs, to obtain a list of all possible 
; results (possibly nil) for each fact, in the format (fact result1 ... resultk)
; If 'filter-out-failures' is non-nil, facts that didn't produce inferences
; are omitted from the results.
;
; We first hide (square-bracket) any ttt-op symbols in the ulfs. Note that
; rules that look for '?', '!', '+', '*', '{}', etc. (which they don't at 
; the time of writing (Nov 7/17)) should be formulated using [?], [!], [+], 
; [*], [{}] etc. At the end we "unhide" the bracketed punctuation.
;
 (cond ((or (null rule-names) (null ulfs)) 
        (return-from results-from-applying-rules nil))
       ((or (atom rule-names) (atom ulfs)) 
        (return-from results-from-applying-rules
          (format nil "** Improper use of 'results-from-applying-rules': ~s, ~s" 
                      rule-names ulfs))))
 (let (ulfs[] result results)
      (setq ulfs[] (mapcar #'hide-ttt-ops ulfs))
      (dolist (ulf ulfs[])
          (setq result 
             (mapcar #'(lambda (f) (apply f (list ulf))) rule-names))
          (if result (push (cons ulf (remove nil result)) results)
              (if (not filter-out-failures) (push (list ulf) results))))
      (reverse results))
 ); end of results-from-applying-rules
          
          

