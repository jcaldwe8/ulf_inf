;;; Gene Kim 7-23-2018
;;;
;;; Initially copied over functions written by Len for the pilot inference
;;; project.  The functions have been refactored for better overall project
;;; organization.



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

(defparameter *infer-want-and-expect-from-request*
;`````````````````````````````````````````````````
; Obtain a list of two inferences from a polite request.
; NB: ttt symbols like !, ?, ~, + etc., appearing in the ULF must
;     first be square-bracketed so as not to "confuse" ttt.
; e.g., "Can somebody help me?
;       (((pres can.aux-v) somebody.pro (help.v me.pro)) ?)
; How to apply: use ttt:apply rule with :shallow nil, max-n 1
;       after square-bracketing '?' or applying hide-ttt-ops
   '(/ (^* ((pres (! aux-indicating-request?))
        (!1 you.pro someone.pro somebody.pro)
        (? please.adv-s) _+))  ; will have to presubst for ?, !
       ((I.pro want.v (that (!1 _+))) ; a list of inferences, here 2
        (I.pro expect.v (that (!1 _+))))))
  ; We'll want to potentially chain from the last part, if it enables
  ; further attitudinal, state change/cessation/inception/continuation
  ; or other inferences.






; Now the functionalized rules themselves (applying the TTT rules at
; multiple depths):

(defun infer-want-from-request (ulf)
;``````````````````````````````````
 (leftmost-rule-result *infer-want-from-request* ulf))


(defun infer-expect-from-request (ulf)
;`````````````````````````````````````
 (leftmost-rule-result *infer-expect-from-request* ulf))


