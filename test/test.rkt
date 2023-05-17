(define (generate-string name)
  (let ((prefix "hello, ")
        (suffix "!"))
    (string-append prefix name suffix)))

(generate-string "Alice")
