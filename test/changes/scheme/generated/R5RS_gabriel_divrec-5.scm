; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 1
; * swapped branches: 0
; * calls to id fun: 0
(letrec ((create-n (lambda (n)
                     @sensitivity:FA
                     (letrec ((loop (lambda (n a)
                                      @sensitivity:FA
                                      (if (= n 0) a (loop (- n 1) (cons () a))))))
                        (loop n ())))))
   (letrec ((recursive-div2 (lambda (l)
                              @sensitivity:FA
                              (<change>
                                 ()
                                 recursive-div2)
                              (if (<change> (null? l) (not (null? l)))
                                 ()
                                 (cons (car l) (recursive-div2 (cddr l)))))))
      (let ((result (__toplevel_cons
                      ()
                      (__toplevel_cons
                         ()
                         (__toplevel_cons
                            ()
                            (__toplevel_cons
                               ()
                               (__toplevel_cons
                                  ()
                                  (__toplevel_cons
                                     ()
                                     (__toplevel_cons
                                        ()
                                        (__toplevel_cons
                                           ()
                                           (__toplevel_cons
                                              ()
                                              (__toplevel_cons
                                                 ()
                                                 (__toplevel_cons
                                                    ()
                                                    (__toplevel_cons
                                                       ()
                                                       (__toplevel_cons
                                                          ()
                                                          (__toplevel_cons
                                                             ()
                                                             (__toplevel_cons
                                                                ()
                                                                (__toplevel_cons
                                                                   ()
                                                                   (__toplevel_cons
                                                                      ()
                                                                      (__toplevel_cons
                                                                         ()
                                                                         (__toplevel_cons
                                                                            ()
                                                                            (__toplevel_cons
                                                                               ()
                                                                               (__toplevel_cons
                                                                                  ()
                                                                                  (__toplevel_cons
                                                                                     ()
                                                                                     (__toplevel_cons
                                                                                        ()
                                                                                        (__toplevel_cons
                                                                                           ()
                                                                                           (__toplevel_cons
                                                                                              ()
                                                                                              (__toplevel_cons
                                                                                                 ()
                                                                                                 (__toplevel_cons
                                                                                                    ()
                                                                                                    (__toplevel_cons
                                                                                                       ()
                                                                                                       (__toplevel_cons
                                                                                                          ()
                                                                                                          (__toplevel_cons
                                                                                                             ()
                                                                                                             (__toplevel_cons
                                                                                                                ()
                                                                                                                (__toplevel_cons
                                                                                                                   ()
                                                                                                                   (__toplevel_cons
                                                                                                                      ()
                                                                                                                      (__toplevel_cons
                                                                                                                         ()
                                                                                                                         (__toplevel_cons
                                                                                                                            ()
                                                                                                                            (__toplevel_cons
                                                                                                                               ()
                                                                                                                               (__toplevel_cons
                                                                                                                                  ()
                                                                                                                                  (__toplevel_cons
                                                                                                                                     ()
                                                                                                                                     (__toplevel_cons
                                                                                                                                        ()
                                                                                                                                        (__toplevel_cons
                                                                                                                                           ()
                                                                                                                                           (__toplevel_cons
                                                                                                                                              ()
                                                                                                                                              (__toplevel_cons
                                                                                                                                                 ()
                                                                                                                                                 (__toplevel_cons
                                                                                                                                                    ()
                                                                                                                                                    (__toplevel_cons
                                                                                                                                                       ()
                                                                                                                                                       (__toplevel_cons
                                                                                                                                                          ()
                                                                                                                                                          (__toplevel_cons
                                                                                                                                                             ()
                                                                                                                                                             (__toplevel_cons
                                                                                                                                                                ()
                                                                                                                                                                (__toplevel_cons
                                                                                                                                                                   ()
                                                                                                                                                                   (__toplevel_cons
                                                                                                                                                                      ()
                                                                                                                                                                      (__toplevel_cons
                                                                                                                                                                         ()
                                                                                                                                                                         (__toplevel_cons
                                                                                                                                                                            ()
                                                                                                                                                                            (__toplevel_cons
                                                                                                                                                                               ()
                                                                                                                                                                               (__toplevel_cons
                                                                                                                                                                                  ()
                                                                                                                                                                                  (__toplevel_cons
                                                                                                                                                                                     ()
                                                                                                                                                                                     (__toplevel_cons
                                                                                                                                                                                        ()
                                                                                                                                                                                        (__toplevel_cons
                                                                                                                                                                                           ()
                                                                                                                                                                                           (__toplevel_cons
                                                                                                                                                                                              ()
                                                                                                                                                                                              (__toplevel_cons
                                                                                                                                                                                                 ()
                                                                                                                                                                                                 (__toplevel_cons
                                                                                                                                                                                                    ()
                                                                                                                                                                                                    (__toplevel_cons
                                                                                                                                                                                                       ()
                                                                                                                                                                                                       (__toplevel_cons
                                                                                                                                                                                                          ()
                                                                                                                                                                                                          (__toplevel_cons
                                                                                                                                                                                                             ()
                                                                                                                                                                                                             (__toplevel_cons
                                                                                                                                                                                                                ()
                                                                                                                                                                                                                (__toplevel_cons
                                                                                                                                                                                                                   ()
                                                                                                                                                                                                                   (__toplevel_cons
                                                                                                                                                                                                                      ()
                                                                                                                                                                                                                      (__toplevel_cons
                                                                                                                                                                                                                         ()
                                                                                                                                                                                                                         (__toplevel_cons
                                                                                                                                                                                                                            ()
                                                                                                                                                                                                                            (__toplevel_cons
                                                                                                                                                                                                                               ()
                                                                                                                                                                                                                               (__toplevel_cons
                                                                                                                                                                                                                                  ()
                                                                                                                                                                                                                                  (__toplevel_cons
                                                                                                                                                                                                                                     ()
                                                                                                                                                                                                                                     (__toplevel_cons
                                                                                                                                                                                                                                        ()
                                                                                                                                                                                                                                        (__toplevel_cons
                                                                                                                                                                                                                                           ()
                                                                                                                                                                                                                                           (__toplevel_cons
                                                                                                                                                                                                                                              ()
                                                                                                                                                                                                                                              (__toplevel_cons
                                                                                                                                                                                                                                                 ()
                                                                                                                                                                                                                                                 (__toplevel_cons
                                                                                                                                                                                                                                                    ()
                                                                                                                                                                                                                                                    (__toplevel_cons
                                                                                                                                                                                                                                                       ()
                                                                                                                                                                                                                                                       (__toplevel_cons
                                                                                                                                                                                                                                                          ()
                                                                                                                                                                                                                                                          (__toplevel_cons
                                                                                                                                                                                                                                                             ()
                                                                                                                                                                                                                                                             (__toplevel_cons
                                                                                                                                                                                                                                                                ()
                                                                                                                                                                                                                                                                (__toplevel_cons
                                                                                                                                                                                                                                                                   ()
                                                                                                                                                                                                                                                                   (__toplevel_cons
                                                                                                                                                                                                                                                                      ()
                                                                                                                                                                                                                                                                      (__toplevel_cons
                                                                                                                                                                                                                                                                         ()
                                                                                                                                                                                                                                                                         (__toplevel_cons
                                                                                                                                                                                                                                                                            ()
                                                                                                                                                                                                                                                                            (__toplevel_cons
                                                                                                                                                                                                                                                                               ()
                                                                                                                                                                                                                                                                               (__toplevel_cons
                                                                                                                                                                                                                                                                                  ()
                                                                                                                                                                                                                                                                                  (__toplevel_cons
                                                                                                                                                                                                                                                                                     ()
                                                                                                                                                                                                                                                                                     (__toplevel_cons
                                                                                                                                                                                                                                                                                        ()
                                                                                                                                                                                                                                                                                        (__toplevel_cons
                                                                                                                                                                                                                                                                                           ()
                                                                                                                                                                                                                                                                                           (__toplevel_cons
                                                                                                                                                                                                                                                                                              ()
                                                                                                                                                                                                                                                                                              (__toplevel_cons
                                                                                                                                                                                                                                                                                                 ()
                                                                                                                                                                                                                                                                                                 (__toplevel_cons
                                                                                                                                                                                                                                                                                                    ()
                                                                                                                                                                                                                                                                                                    (__toplevel_cons
                                                                                                                                                                                                                                                                                                       ()
                                                                                                                                                                                                                                                                                                       (__toplevel_cons
                                                                                                                                                                                                                                                                                                          ()
                                                                                                                                                                                                                                                                                                          (__toplevel_cons
                                                                                                                                                                                                                                                                                                             ()
                                                                                                                                                                                                                                                                                                             (__toplevel_cons
                                                                                                                                                                                                                                                                                                                ()
                                                                                                                                                                                                                                                                                                                (__toplevel_cons
                                                                                                                                                                                                                                                                                                                   ()
                                                                                                                                                                                                                                                                                                                   (__toplevel_cons () (__toplevel_cons () (__toplevel_cons () (__toplevel_cons () ()))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
         (equal? (recursive-div2 (create-n 200)) result))))