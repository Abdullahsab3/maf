(define string1 "This is a test for a string")

"I am just free. so set me free"

(define (return-the-string)
  (define string2 "gebonden aan een variabele string1")
  "I am hanging though, so I could be garbage"
  string1)

(define (call-the-other-proc)
  (define the-non-garbage-string (return-the-string))
  (define string3 "gebonden aan een variabele string3")
  (string-append the-non-garbage-string string3))

  ;; wat er zeker weg moet: string@2, string@7
  ;; wat er misschien weg moet: string@6

(call-the-other-proc)