(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (lambda? exp) (tagged-list? exp 'lambda))

(define (which-expression exp)
    (cond ((lambda? exp) "A lambda expression")
          (else          "Some other expression")))

(which-expression (make-lambda '(x y) '(+ x y)))