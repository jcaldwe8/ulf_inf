;;positive polarity application removed
(setq fracas5p
    '(((The.d ((really.adv-a ambitious.a) (plur tenor.n)))
        ((pres be.v) Italian.a)) \.))
(setq fracas5h
    '((There.pro ((pres be.v)
        (n+preds ((really.adv-a ambitious.a) (plur tenor.n))
            (who.rel ((pres be.v) Italian.a))))) \.))

(setq fracas6p
    '(((No.d ((really.adv-a great.a) (plur tenor.n)))
        ((pres be.v) modest.a)) \.))
(setq fracas6h
    '((There.pro ((pres be.v)
        (n+preds ((really.adv-a great.a) (plur tenor.n))
            (who.rel ((pres be.v) modest.a))))) \.))

(setq fracas16p
    '((((nquan (at_most.adv-a two.a)) (plur tenor.n))
        ((pres will.aux-s) (contribute.v
            (their.d (plur fee.n))
            (adv-a (to.p (k charity.n)))))) \.))
(setq fracas16h
    '((There.pro ((pres be.v)
        (n+preds (plur tenor.n)
            (who.rel ((pres will.aux-s) (contribute.v
                (their.d (plur fee.n))
                (adv-a (to.p (k charity.n))))))))) \.))


(setq fracas-test0
    '(The.d (plur tenor.n)))
(setq fracas-test1
    '((There.pro ((pres be.v)
        (n+preds ((really.adv-a ambitious.a) (plur tenor.n))
            (that.rel ((pres be.v) Italian.a))))) \.))

;; Polarize formula with all positive polarity.
(defun all-positive (f)
  (cond
    ((null f) f)
    ((atom f) (list '+ f))
    (t (list '+
             (mapcar #'all-positive f)))))



(setq p-fracas5p
  '(+ (+ ((+ ((+ The.d) (+ ((+ ((+ really.adv-a) (+ ambitious.a))) 
                          (+ ((+ plur)  (+ tenor.n)))))))
        (+ ((+ ((+ pres) (+ be.v))) (+ Italian.a))))) (+ \.)))
(setq p-fracas-test1
    '(+ ((+ There.pro) (+ ((+ ((+ pres) (+ be.v)))
        (+ ((+ :l) (+ x1) (+ ((+ and.cc) 
                              (+ ((+ x1) (+ ((+ ((+ really.adv-a) (+ ambitious.a))) 
                                            (+ ((+ plur) (+ tenor.n)))))))     
                              (+ ((+ x1) (+ ((+ ((+ pres) (+ be.v))) 
                                             (+ Italian.a))))))))))))))
(setq p-fracas-test2
    '(+ ((+ There.pro) (+ ((+ ((+ pres) (+ be.v)))
        (+ ((+ :l) (+ x1) (+ ((+ and.cc) 
                              (+ ((+ x1) (+ ((+ ((+ really.adv-a) (+ ambitious.a))) 
                                            (+ ((+ plur) (+ tenor.n)))))))     
                              (+ ((+ x1) (+ happy.a))))))))))))
                                        
                                         ;(+ ((+ ((+ pres) (+ be.v))) 
                                         ;    (+ Italian.a))))))))))))))
(setq fracas5pp fracas5p)
(setq fracas5hp fracas5h)
(setq fracas6pp fracas6p)
(setq fracas6hp fracas6h)
(setq fracas16pp fracas16p)
(setq fracas16hp fracas16h)
(setq fracas-test0p fracas-test0)
(setq fracas-test1p fracas-test1)

;;(setq fracas5pp (all-positive fracas5p))
;;(setq fracas5hp (all-positive fracas5h))
;;(setq fracas6pp (all-positive fracas6p))
;;(setq fracas6hp (all-positive fracas6h))
;;(setq fracas16pp (all-positive fracas16p))
;;(setq fracas16hp (all-positive fracas16h))
;;(setq fracas-test0p (all-positive fracas-test0))
;;(setq fracas-test1p (all-positive fracas-test1))



