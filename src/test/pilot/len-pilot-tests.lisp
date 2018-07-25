;;; Gene Kim 7-24-2018
;;; Ported from formulas defined by Len to test the pilot inference system.

;; TODO (GK 7-25-2018): change to "want you to close the door" (much more natural)
;;                      sim. for other 'want' examples
;; TODO (GK 7-25-2018): change -cf to (cf ..), similarly update other anntation
;;                      guideline changes.

;; Define applicable rule subset for request and counterfactuals that we want
;; to test.
(defparameter *rule-names* 
 '(infer-want-from-request infer-expect-from-request 
   infer-falsehood-from-positive-counterfactual 
   infer-falsehood-from-inverted-positive-counterfactual
   infer-fact-from-negative-counterfactual
   infer-fact-from-inverted-negative-counterfactual
 ))
;; Run the selected subset of rules on the given ULF and return a list of:
;; (ulf result1 result2 ...)
(defun run-subset-rules (ulf)
  (mapcar #'unhide-ttt-ops
          (car (results-from-applying-rules *rule-names* (list ulf) t))))

;; Helper function to assert equality between lists, one by one.
;; TODO: make the exact assert function an argument for generalizability.
(defun list-assert-equal (result expect)
  (assert-equal (length result) (length expect) 
                (length result) (length expect)
                result expect)
  (mapcar #'(lambda (x) (assert-equal (first x) (second x)
                                      (first x) (second x)))
          (mapcar #'list result expect)))

;; Macro to reduce the size of the test declarations.
;; Arguments
;;  name:     The name of the test (as a symbol, i.e. unquoted)
;;  sentence: String surface sentence being tested
;;  ulf:      Source ULF
;;  expected: List of expected output ULFs (in order TODO: make the order not
;;            matter)
(defmacro define-len-pilot-subset-test (name sentence ulf expected)
  `(define-test ,name
     ,(format nil "Subset rule test on the sentence '~a'" sentence) 
     (:tag :rq :subset-rules :len-pilot)
     (let ((result (run-subset-rules ,ulf))
           (ulf ,ulf)
           (expected ,expected))
        (assert-equal (first result) ulf 
                      (first result) ulf)
        (list-assert-equal (cdr result) expected))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define tests.
;;
;; This section defines tests specifically for the same subset of rules that
;; were used in the pilot inference task.  First, simple tests are listed, 
;; followed by sentences sampled from Tatoeba.
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Simple tests (thought up).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-len-pilot-subset-test 
  test-subset-request1
  "Would you please speak up"
  '((pres would.aux-v) you.pro please.adv-s speak_up.v)
  '((i.pro ((pres want.v) (that (you.pro speak_up.v))))
    (i.pro ((pres expect.v) (that (you.pro speak_up.v))))))
  
(define-len-pilot-subset-test
  test-subset-request2
  "Can somebody help me?"
  '(((pres can.aux-v) somebody.pro (help.v me.pro)) ?)
  '((i.pro ((pres want.v) (that (somebody.pro (help.v me.pro)))))
    (i.pro ((pres expect.v) (that (somebody.pro (help.v me.pro)))))))

(define-len-pilot-subset-test
  test-subset-request3
  "Please will you close the door when you go out"
  '((Please.adv-s
      ((pres will.aux-s) you.pro (close.v (the.d door.n)
       (adv-e (when.ps (you.pro ((pres go.v) out.adv-a))))))))
  '((i.pro ((pres want.v)
       (that (you.pro (close.v (the.d door.n)
             (adv-e (when.ps (you.pro ((pres go.v) out.adv-a)))))))))
    (i.pro ((pres expect.v)
       (that (you.pro (close.v (the.d door.n)
              (adv-e (when.ps (you.pro ((pres go.v) out.adv-a)))))))))))

(define-len-pilot-subset-test
  test-subset-request4
  "Could you turn on the light please"
  '((((pres could.aux-v) you.pro (turn_on.v (the.d light.n)))
     please.adv-s) ?)
  '((i.pro ((pres want.v) (that (you.pro (turn_on.v (the.d light.n))))))
    (i.pro ((pres expect.v) (that (you.pro (turn_on.v (the.d light.n))))))))

(define-len-pilot-subset-test
  test-subset-wish1
  "I wish he would finish it"
  '(i.pro ((pres wish.v) (tht (he.pro (would-cf.aux-s (finish.v it.pro))))))
  '((he.pro ((pres will.aux-s) not.adv-s (finish.v it.pro)))))

(define-len-pilot-subset-test
  test-subset-wish2
  "I wish he would leave"
  '(i.pro ((pres wish.v) (tht (he.pro (would-cf.aux-v leave.v)))))
  '((he.pro ((pres will.aux-v) not.adv-s leave.v))))

(define-len-pilot-subset-test
  test-subset-if1
  "If I were rich I would travel to Rome"
  '(if.c (I.pro (were-cf.v rich.a))
         (i.pro ((pres would.aux-v) (travel.v (to-arg.p |Rome|)))))
  '((i.pro ((pres be.v) not.adv-s rich.a))))

(define-len-pilot-subset-test
  test-subset-if2
  "If she had a hammer she would swing it"
  '(if.c (she.pro (had-cf.v (a.d hammer.n)))
         (she.pro ((pres would.aux-v (swing.v it.pro)))))
  '((she.pro ((pres do.aux-s) not.adv-s (have.v (a.d hammer.n))))))

(define-len-pilot-subset-test
  test-subset-if3
  "If she had had a hammer she would swing it"
  '(if.c (she.pro (perf-cf (have.v (a.d hammer.n))))
         (she.pro ((past would.aux-v) ((perf swing.v) it.pro))))
  '((she.pro ((past do.aux-s) not.adv-s (have.v (a.d hammer.n))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tatoeba Sampled Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; If-then constructions.

(define-len-pilot-subset-test
  test-subset-if-17577
  "If I were you I would be able to succeed."
  '(((adv-s (If.ps (I.pro (were-cf.v you.pro))))
    (I.pro ((pres would.aux-s) (be.v (able.v (to succeed.a)))))) \.)
  '((i.pro ((pres be.v) not.adv-s you.pro))))

(define-len-pilot-subset-test
  test-subset-if-17955
  "If you were to fall from that bridge, it would be almost impossible to rescue you."
  '(((adv-s (If.ps (you.pro (were-cf.v (to (fall.v (adv-a (from.p 
      (that.d bridge.n))))))))) \, (it.pro ((pres would.aux-s) (be.v 
        (almost.adv-a (impossible.a (adv-a (to.p (rescue.v you.pro))))))))) \.)
  '((you.pro
     ((pres be.v) not.adv-s
      (to (fall.v (adv-a (from.p (that.d bridge.n))))))))) ; pretty close to correct

(define-len-pilot-subset-test
  test-subset-if-18636
  "If I had money, I would pay what I owe you."
  '(((adv-s (If.ps (I.pro (had-cf.v (k money.n))))) \,
    (I.pro ((pres would.aux-s) (pay.v (sub what.pro (I.pro 
                              ((pres owe.v) you.pro *h))))))) \.)
  '((i.pro ((pres do.aux-s) not.adv-s (have.v (k money.n))))))

(define-len-pilot-subset-test
  test-subset-if-16988
  "If only I had taken your advice."
  '(((adv-s (If.ps (only.adv-s (I.pro (perf-cf (take.v (your.d advice.n))))))) {ref1}.s) \.)
  '((i.pro ((past do.aux-s) not.adv-s (take.v (your.d advice.n))))))

;;; Inverted if-then constructions.

(define-len-pilot-subset-test
  test-subset-inv-16968
  "Had I known your telephone number, I would have called you"
  '(((adv-s (perf-cf I.pro (know.v (your.d (telephone.n number.n))))) \,
    (I.pro ((past would.aux-s) (perf (call.v you.pro))))) \.)
  '((i.pro
    ((past do.aux-s) not.adv-s
     (know.v (your.d (telephone.n number.n)))))))
  
(define-len-pilot-subset-test
  test-subset-inv-16968-v2
  "Had I known your telephone number, I would have called you."
  '(({if}.c (I.pro (perf-cf (know.v (your.d (telephone.n number.n)))))
            (I.pro ((past would.aux-s) (perf (call.v you.pro))))) \.)
  '((i.pro
    ((past do.aux-s) not.adv-s
     (know.v (your.d (telephone.n number.n)))))))

(define-len-pilot-subset-test
  test-subset-inv-18529
  "Were I rich, I would help the poor."
  '(((adv-s (Were-cf.v I.pro rich.a)) \,
    (I.pro ((pres would.aux-s) (help.v (the.d (poor.a {ref1}.n)))))) \.)
  '((i.pro ((pres be.v) not.adv-s rich.a))))

(define-len-pilot-subset-test
  test-subset-inv-18529-v2
  "I were rich I would help the poor."
  '(({if}.c (I.pro (Were-cf.v rich.a)) 
           (I.pro ((pres would.aux-s) (help.v (the.d (poor.a {ref1}.n)))))) \.)
  '((i.pro ((pres be.v) not.adv-s rich.a))))

;;; Requests.

(define-len-pilot-subset-test
  test-subset-req-1661
  "Could you dial for me?"
  '(((pres Could.aux-v) you.pro ((dial.v {ref1}.pro) (adv-a (for.p me.pro)))) ?)
  '((i.pro
      ((pres want.v)
       (that (you.pro ((dial.v {ref1}.pro) (adv-a (for.p me.pro)))))))
    (i.pro
      ((pres expect.v)
       (that (you.pro ((dial.v {ref1}.pro) (adv-a (for.p me.pro)))))))))
  
(define-len-pilot-subset-test
  test-subset-req-2242
  "Could you please repeat that?"
  '(((pres Could.aux-v) you.pro please.adv-s (repeat.v that.pro)) ?)
  '((i.pro ((pres want.v) (that (you.pro (repeat.v that.pro)))))
    (i.pro ((pres expect.v) (that (you.pro (repeat.v that.pro)))))))

;;; Wish constructions.

(define-len-pilot-subset-test
  test-subset-wish-1393
  "I wish I could go to Japan."
  '((I.pro ((pres wish.v)
          (tht (I.pro ((pres could.aux-v)
                        (go.v (adv-a (to.p |Japan|)))))))) \.)
  '())

(define-len-pilot-subset-test
  test-subset-wish-1393-v2
  "I wish I could go to Japan."
  '((I.pro ((pres wish.v)
           (tht (I.pro (could-cf.aux-v 
                        (go.v (adv-a (to.p |Japan|)))))))) \.)
  '((i.pro ((pres can.aux-v) not.adv-s (go.v (adv-a (to.p |Japan|)))))))

(define-len-pilot-subset-test
  test-subset-wish-2470
  "I wish she would stop playing that stupid music."
  '((I.pro ((pres wish.v)
          (tht (she.pro ((pres would.aux-s) ; NB: the incorrect annotation
                          (stop.v (ka (play.v (that.d (stupid.a music.n)))))))))) \.)
  '())

(define-len-pilot-subset-test
  test-subset-wish-2470-v2
  "I wish she would stop playing that stupid music."
  '((I.pro ((pres wish.v)
          (tht (she.pro (would-cf.aux-s
                          (stop.v (ka (play.v (that.d (stupid.a music.n)))))))))) \.)
  '((she.pro
      ((pres will.aux-s) not.adv-s
                         (stop.v (ka (play.v (that.d (stupid.a music.n)))))))))

