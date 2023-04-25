(define (return-the-string)
  "hello")
(define x '())

(define (another-dep)
  (define a (return-the-string))
  (set! x a)
  (string-append a a))

(define (test)
  (define b (string-append (another-dep) (return-the-string)))
  b)
  
(test)