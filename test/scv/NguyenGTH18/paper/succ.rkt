#lang racket

(define (succ n) (add1 n))

(provide/contract
 [succ (->i ([n (and/c integer? (>=/c 0))])
            (res (n) (and/c integer? (>/c n))))])
