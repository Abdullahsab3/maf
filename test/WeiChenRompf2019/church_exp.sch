((lambda (church=?)
   ((lambda (church0?)
      ((lambda (church3)
         ((lambda (church2)
            ((lambda (church1)
               ((lambda (church0)
                  ((lambda (sub)
                     ((lambda (pred)
                        ((lambda (mult)
                           ((lambda (plus)
                              ((lambda ($0)
                                 ((lambda ($1)
                                    ((lambda ($2)
                                       ((lambda ($3)
                                          ((lambda ($4)
                                             ((lambda ($5)
                                                ((lambda ($6)
                                                   ((lambda ($7)
                                                      ((lambda ($8)
                                                         ((lambda ($9)
                                                            ((church=? ((mult church2) ((plus church1) church3)))
                                                             ((plus ((mult church2) church1)) ((mult church2) church3))))
                                                          (set! church=? (lambda (e1) 
                                                                           (lambda (e2)
                                                                              (if (church0? e1)
                                                                                  (church0? e2)
                                                                                  (if (church0? e2) #f
                                                                                      ((church=? ((sub e1) church1)) ((sub e2) church1)))))))))
                                                       (set! church0? (lambda (z) ((z (lambda (zx) #f)) #t)))))
                                                    (set! church3 (lambda (f3) (lambda (x3) (f3 (f3 (f3 x3))))))))
                                                 (set! church2 (lambda (f2) (lambda (x2) (f2 (f2 x2)))))))
                                              (set! church1 (lambda (f1) (lambda (x1) (f1 x1))))))
                                           (set! church0 (lambda (f0) (lambda (x0) x0)))))
                                        (set! sub (lambda (s1) (lambda (s2) ((s2 pred) s1))))))
                                     (set! pred (lambda (n) (lambda (rf) (lambda (rx) (((n (lambda (g) (lambda (h) (h (g rf))))) (lambda (ignored) rx)) (lambda (id) id))))))))
                                  (set! mult (lambda (m1) (lambda (m2) (lambda (mf) (m2 (m1 mf))))))))
                               (set! plus (lambda (p1) (lambda (p2) (lambda (pf) (lambda (x) ((p1 pf) ((p2 pf) x)))))))))
                            (void)))
                         (void)))
                      (void)))
                   (void)))
                (void)))
             (void)))
          (void)))
       (void)))
    (void)))
 (void))