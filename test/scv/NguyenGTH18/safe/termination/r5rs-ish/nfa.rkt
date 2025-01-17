#lang racket/base
(require soft-contract/fake-contract)

; The recursive-nfa benchmark.  (Figure 45, page 143.)

;; Changed by Matthew 2006/08/21 to move string->list out of the loop

  
(define (recursive-nfa input)
  
  (define (state0 input)
    (or (state1 input) (state3 input) #f))
  
  (define (state1 input)
    (and (not (null? input))
         (or (and (char=? (car input) #\a)
                  (state1 (cdr input)))
             (and (char=? (car input) #\c)
                  (state1 (cdr input)))
             (state2 input))))
  
  (define (state2 input)
    (and (not (null? input))
         (char=? (car input) #\b)
         (not (null? (cdr input)))
         (char=? (cadr input) #\c)
         (not (null? (cddr input)))
         (char=? (caddr input) #\d)
         'state2))
  
  (define (state3 input)
    (and (not (null? input))
         (or (and (char=? (car input) #\a)
                  (state3 (cdr input)))
             (state4 input))))
  
  (define (state4 input)
    (and (not (null? input))
         (char=? (car input) #\b)
         (not (null? (cdr input)))
         (char=? (cadr input) #\c)
         'state4))

  (or (state0 input)
      'fail))

(provide
 (contract-out
  [recursive-nfa ((listof char?) . -> . any/c #:total? #t)]))


