; Gene Kim 7-23-2018
; Inference functions largely based on Len's inference.lisp,
; and inference-rules-and-helpers.lisp from the pilot
; inference project.

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
          
          
