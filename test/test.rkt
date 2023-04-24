(define (return-the-string)
  "hello")
(define x '())

(define (call-the-other-proc)
  (return-the-string))

(define (another-dep)
  (define a (return-the-string))
  (set! x a)
  (string-append a a))

(string-append (another-dep) (return-the-string))