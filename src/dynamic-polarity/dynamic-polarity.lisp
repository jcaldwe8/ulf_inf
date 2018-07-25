; Gene Kim 7-17-2018
; Functions for get polarity annotations from a ULF formula dynamically.  
;
; These functions currently rely on using the Stanford CoreNLP polarity
; annotations from the NatLog project.  This is retrieved by running the java
; program.  Please make sure you have java appropriately configured and 
; have the necessary corenlp jars.  run_natlog.sh is the simple script that
; we use to call the java program and shows what dependencies are needed.


;; Memoization parameters.
(defparameter *run-natlog-memo* (make-hash-table :test #'equal))

;; Polarity markings we will use.
(defparameter *polarities*
  '(+ - o))

;; Takes a symbol and returns whether the symbol has a .-delimited suffix.
(defun has-suffix? (s)
  (> (length (cl-strings:split (format nil "~s" s) "."))
     1))

;; Converts a symbol to the corresponding string.
(defun sym2str (sym)
  (format nil "~s" sym))

;; Returns t is s is strictly a ULF name, e.g. |John|, |Mary|, etc.
;; Returns false on name-like predicates (e.g. |Green River|.n).
;; Can take a symbol or a string.
;; TODO: separate into separate functions for symbol and string since the ULF can contain strings!
(defun is-strict-name? (s)
  (let* ((sstr (if (symbolp s) (sym2str s) s))
         (chars (coerce sstr 'list)))
    (and (eql #\| (nth 0 chars))
         (eql #\| (nth (1- (length chars)) chars)))))

;; Post formats a ULF-to-string mapping.  If it is a name (e.g. |John|), the 
;; pipes are stripped off.  Otherwise, the string is made lowercase.
(defun post-format-ulf-string (s)
  (if (is-strict-name? s)
    (coerce (subseq (coerce s 'list) 1 (1- (length s))) 
            'string)
    (string-downcase s)))

;; Strips the suffix, marked with "." from a string.
;;  e.g. "man.n" -> "man"
;; If there are multiple periods in the string, only the substring after the
;; last period is stripped off.
(defun strip-suffix (s)
  (let ((split (cl-strings:split s ".")))
    (cl-strings:join 
      (subseq split 0 (max 1 (1- (length split))))
      :separator ".")))

;; Returns t if 'token' is an atomic ULF element that has a corresponding token
;; in the surface string and return nil otherwise.  e.g.,
;;   man.n -> t
;;   that -> t
;;   tht -> nil
;;   k -> nil
;;   to -> t
;;   perf -> t
(defun is-surface-token? (token)
  (or (has-suffix? token)
      (is-strict-name? token)
      (member token'(that not and or to))))


;; Maps a ULF formula to a corresponding surface string.
;; NB: currently this is incomplete and not fluent.
(defun ulf-to-string (ulf)
  ;; TODO: make more sophisticated version (tenses, quotes, perfect, passive, prog, etc.).

  ;; For now just drop all special operators and just take the suffixed tokens.
  ;; The only non-suffixed tokens that we preserve are "that", "not", "and",
  ;; "or", "to".  
  ;; TODO: just have a canonicalization function for introducing implicit
  ;; suffixes and another canonicalization function for identifying words that
  ;; appear in the surface form.
  (let* ((surface-only (remove-if-not #'is-surface-token?
                                      (alexandria:flatten ulf)))
         (stringified (mapcar #'sym2str surface-only))
         ;(dotsplit (mapcar #'(lambda (x) (cl-strings:split x ".")) stringified))
         ;(pruned (mapcar #'(lambda (x) (subseq x 0 (max 1 (1- (length x))))) dotsplit))
         ;(rejoined (mapcar #'(lambda (x) (cl-strings:join x :separator ".")) pruned))
         (rejoined (mapcar #'strip-suffix stringified))
         (postform (mapcar #'post-format-ulf-string rejoined)))
    (cl-strings:join postform :separator " ")))


;; run-natlog takes a list of sentence strings and obtains polarity annotations
;; at the token level via the Stanford CoreNLP NatLog system.  The returned
;; value is a list of pairs, where each pair is the token string followed by
;; the polarity, e.g. 
;;  Input: '("all cats have tails" 
;;           ...)
;;  Output: '((("all" +) ("cats" -) ("have" +) ("tails" +)) 
;;            ...)
;; NB: The input strings MUST NOT contain any newlines.  Newlines will cause
;;     the sentence to be treated as separate sentences.
(defun run-natlog (strlst &key (memoize t))
  ;; 1. Write strings for processing to temporary file.
  ;; 2. Run shell script.
  ;; 3. Read in tokenized string and annotations.
  ;; 4. Reformat into desired format.
  (let* ((tempfile-raw ".natlog_raw.temp")
         (tempfile-ann ".natlog_ann.temp")
         raw-fh ann-fh natlog-stranns newstrs)
    
    ;; 1. Write string to temporary file.
    (setq newstrs (if memoize 
                    (remove-if #'(lambda (s) (gethash s *run-natlog-memo*)) 
                               strlst)
                    strlst))
    (when newstrs
      (progn
        (setq raw-fh (open tempfile-raw :direction :output))
        (dolist (str newstrs)
          (write-line str raw-fh))
        (close raw-fh)
        ;; 2. Run shell script
        ;; TODO: suppress shell output (or make a flag for it).
        (run-shell-command (format nil 
                                   "./run_natlog.sh ~a ~a" 
                                   tempfile-raw tempfile-ann))
        ;; 3. Read in tokenized string and annotations
        (setq ann-fh (open tempfile-ann))
        (setq natlog-stranns 
              (mapcar #'(lambda (x) (list (read-line ann-fh)
                                          (read-line ann-fh)))
                      strlst))
        (close ann-fh)
        ;; Delete files.
        (delete-file tempfile-raw)
        (delete-file tempfile-ann)))
    
    ;; 3b. Save new memos and lookup previous memos to combine with new output.
    (if memoize
      (dolist (nl-strann natlog-stranns)
        (let ((nl-str (first nl-strann))
              (nl-ann (second nl-strann)))
          (setf (gethash nl-str *run-natlog-memo*) nl-ann))))
    (if memoize
      (setq natlog-stranns
            (mapcar #'(lambda (s) (list s (gethash s *run-natlog-memo*)))
                    strlst)))

    ;; 4. Reformat into desired format.
    ;; Words (as tokenized by NatLog) are paired with the corresponding
    ;; polarity, where the polarity is represented as one of [+,-,o].
    (mapcar 
      #'(lambda (strann)
          (let ((natlog-str (first strann))
                (natlog-ann (second strann)))
            (mapcar #'list 
                    (cl-strings:split (cl-strings:clean natlog-str))
                    (mapcar #'(lambda (natlog-pol)
                                (cond ((equal natlog-pol "down") '-)
                                      ((equal natlog-pol "up") '+)
                                      ((equal natlog-pol "flat") 'o)))
                            (cl-strings:split 
                              (cl-strings:clean natlog-ann))))))
      natlog-stranns)))

;; Run NatLog on a list of ULFs.
(defun run-natlog-ulfs (ulfs)
  (run-natlog (mapcar #'ulf-to-string ulfs)))

;; Aligns the atomic elements in 'ulf' with the polarity annotations from 'pol'.
;; The alignment produces a polarity annotated ULF formula with full semantic,
;; but partial symbolic coverage. e.g.,
;;   ulf: '((ALL.D (PLUR CAT.N)) ((PRES HAVE.V) (K (PLUR TAIL.N)))))
;;   pol: '(("all" +) ("cat" -) ("have" +) ("tail" +))
;;   return: (((ALL.D +) (PLUR (CAT.N -))) ((PRES (HAVE.V +)) (K (PLUR (TAIL.N +)))))
;;
;; Notice that, only the polarities of tokens corresponding to annotations from
;; 'pol' are annotated, but polarity for the remaining atomic elements and all
;; the compositional elements can be inferred from the existing polarities.
(defun align-ulf-polarity (ulf pol)
  ;; For now we just to greedy search.
  ;; Depth-first search along ULF.  For each token in the ULF, check if it's a
  ;; surface token.  If so pair with next polarity if the next natlog token
  ;; is a subseq of the ulf token.  Since NatLog doesn't merge tokens, they
  ;; will always have the shortened version of the token.
  ;; TODO: need to handle underscored tokens... and strings?
  (labels
    ((rec-helper (u p) 
       (cond 
         ;; If nil, return (nil pol)
         ((null u)
          (list nil p))

         ;; If token, do token comparison.
         ((and (atom u) (is-surface-token? u))
          (let ((ptok (caar p))
                (pp (cadar p))
                (procu (post-format-ulf-string (strip-suffix (sym2str u)))))
            ;; If the NatLog version is a subsequence, then mark the current
            ;; ULF with the polarity.  Otherwise, just return the ulf.
            (if (search ptok procu)
              (list (list u pp) (cdr p))
              (list u p))))

         ;; If token, but not surface, just return.
         ((atom u) (list u p))
         ;; Otherwise, recursion!
         (t
           (let* ((lrec (rec-helper (car u) p))
                  (lu (first lrec))
                  (lp (second lrec))
                  (rrec (rec-helper (cdr u) lp))
                  (ru (first rrec))
                  (rp (second rrec)))
             (list (cons lu ru) rp)))))) ; end of labels

    ;; Strip off the returned remaining polarity.
    (first (rec-helper ulf pol)))) ; end of align-ulf-polarity
 
;; TODO: put this in a test file.
;; Test with the following has worked so far.
;; NB: since we're assuming the transparent operators aren't interacting, we
;; can merge them with their children in terms of polarity (e.g. (plur (cat.n
;; +)) -> ((plur +) (cat.n +))).
(setq ulf1 '(|Mary| ((PRES KNOW.V) (THAT (|John| ((PAST GO.V) (K HOME.N)))))))
;(setq nlog1 (run-natlog (list (ulf-to-string ulf1))))
;(setq aligned1 (align-ulf-polarity ulf1 nlog1))
;
(setq ulf2 '((ALL.D (PLUR CAT.N)) ((PRES HAVE.V) (K (PLUR TAIL.N)))))
;(setq nlog2 (run-natlog (list (ulf-to-string ulf2))))
;(setq aligned2 (align-ulf-polarity ulf2 nlog2))

;; Infers polarities for a complete ULF given polarity annotations of tokens
;; corresponding to surface strings.
;;
(defun infer-polarities (ulf)
  (labels
    (
     ;; Returns t if x is a polarized item.
     (polar? (x)
       (and (listp x) (= (length x) 2)
            (member (second x) *polarities*)))
     ;; Returns t if x is a polarized atomic element.
     (polar-atom? (x)
       (and (polar? x) (atom (first x))))
     ;; Polarities are propagated up the tree from the right, and down to the
     ;; left.  Assumes all right-most left-nodes are polarity marked.
     ;;   1. Recurse right-most and get polarity.
     ;;   2. Recurse to other children with polarity.
     (updown-helper (f parentpol)
       (cond 
         ;; Error condition.
         ((and (atom f) (null parentpol))
          (error "updown-helper precondition starting polarity annotations has failed"))
         ;; Polarized atom, return as is.
         ((polar-atom? f) f)
         ;; Atom otherize, take parent polarity.
         ((atom f) (list f parentpol))
         ;; Recursive case.  Take right-most child's polarity if without one,
         ;; then propagate to other children.
         (t (let* (;; Get current polarity -- nil if None.
                   (curpol (if (polar? f)
                             (car (last f)) 
                             nil))
                   ;; Remove polarity if one exists.
                   (npf (if curpol (second f) f))
                   ;; Recurse to right.
                   (rrec (updown-helper (car (reverse npf)) nil))
                   (rpol (second rrec))
                   ;; Update polarity.
                   (newpol (if curpol curpol rpol))) 
              ;; Recurse to rest and return.
              (list
                (reverse (cons rrec      
                               (mapcar #'(lambda (x) 
                                           (updown-helper x newpol)) 
                                       (cdr (reverse npf)))))
                newpol)))))
     ) ; end of labels defs

    ;; TODO: write function that preprocesses 'ulf' to ensure that if follows
    ;; the precondition for updown-helper
    (updown-helper ulf nil)))


;; Annotates the ULF with polarity.  The polarity annotations are
;; incomplete so some segments 
(defun annotate-polarity (ulf)
  (infer-polarities         ; complete polarity coverage
    (align-ulf-polarity    ; align text polarity to ulf
      ulf
      (first (run-natlog           ; get text token polarity
        (list (ulf-to-string ulf)))))))

;; Returns the polarity for the given segment of the ulf and its parent.
;; NB: 'segment' and 'parent' must be referentially equivalent to the
;;      corresponding segments within the 'fullulf'.  This is necessary to 
;;      ensure correctness when there are equivalent subsegments in different 
;;      contexts, e.g.
;;       I am a person, but Spot is not a person.
;;     The first occurrence is in a positive context (we can substitute in
;;     'living thing' for it), while the second occurrence is in a negative
;;     context (same substitution is not possible).
;; e.g.
;;  segment:  'cat.n
;;  parent:   '(all.d cat.n)
;;  fullulf:  '((all.d cat.n) ((pres have.v) (k (plur tail.n))))
;;  return:   '-
;;
;; If 'segment' is the root, please supply nil as 'parent'.
(defun get-segment-polarity (segment parent fullulf)
  (labels
    (
     ;; Searches rawulf and polulf for segment that matches in rawulf and 
     ;; returns the corresponding polarity in polulf.  If no match is found,
     ;; returns nil.
     (find-segment (rawulf parulf polulf)
       (let* ((curpol (second polulf))
              (nopolulf (first polulf)))
         ;; Check that the parallel versions are coherent.
         (assert (or
                   (and (atom rawulf) (atom nopolulf) (equal rawulf nopolulf))
                   (equal (length rawulf) (length nopolulf))))
         (cond
           ;; Found a match!
           ((and (eq segment rawulf)
                 (eq parent parulf)) curpol)
           ;; Base case.
           ((atom rawulf) nil)
           ;; Recursive case.
           (t (reduce #'(lambda (acc new) 
                          (if acc acc
                            (apply #'find-segment 
                                   (list (first new) rawulf (second new)))))
                      (mapcar #'list rawulf nopolulf) ; zip raw and polar
                      :initial-value nil)))))
     ) ; end of labels defs

  ;; 1. Get polarity alignment of the ULF, 'pulf'
  ;; 2. Parallel search of 'fullulf' and 'pulf' for 'segment' and get polarity
  (find-segment fullulf
                nil
                (annotate-polarity fullulf))))


(setq ulfpart1 '((PRES HAVE.V) (K (PLUR TAIL.N))))
(setq ulfpart2 'CAT.N)
(setq ulfpart2-par (list 'plur ulfpart2))
(setq ulfpart3 (list 'all.d ulfpart2-par))
(setq compulf (list ulfpart3 ulfpart1))
(setq diffulf '((ALL.D (PLUR CAT.N)) ((PRES HAVE.V) (K (PLUR TAIL.N)))))
; Expected output from these.
; (get-segment-polarity ulfpart1 compulf compulf)
; +
; (get-segment-polarity ulfpart1 compulf diffulf)
; NIL
; (get-segment-polarity ulfpart1 diffulf compulf)
; NIL
; (get-segment-polarity ulfpart1 diffulf diffulf)
; NIL
; (get-segment-polarity ulfpart2 ulfpart2-par compulf)
; -
; (get-segment-polarity ulfpart2 ulfpart2-par diffulf)
; NIL
; (get-segment-polarity ulfpart2 nil compulf)
; NIL

