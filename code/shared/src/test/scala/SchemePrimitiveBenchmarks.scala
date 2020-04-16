package scalaam.test.language.scheme

import scalaam.language.scheme.{SchemeExp, SchemeLattice}
import scalaam.modular.ModAnalysis
import scalaam.modular.scheme.SchemeModFSemantics

object SchemePrimitiveBenchmarks {

  type Analysis = ModAnalysis[SchemeExp] with SchemeModFSemantics
  type V = (ModAnalysis[SchemeExp] with SchemeModFSemantics)#Value
  type A = (ModAnalysis[SchemeExp] with SchemeModFSemantics)#Addr
  type P = (ModAnalysis[SchemeExp] with SchemeModFSemantics)#Prim
  type Env = (ModAnalysis[SchemeExp] with SchemeModFSemantics)#Component
  type L = SchemeLattice[V, A, P, Env]

  val bench: List[(String, L => V)] = List(
    ("(eq? 'a 'a)", _.bool(true)),
    ("(eq? (cons 'a '()) (cons 'a '()))", _.bool(false)),
    ("(eq? (list 'a) (list 'a))", _.bool(false)),
    ("(eq? '() '())", _.bool(true)),
    ("(eq? car car)", _.bool(true)),
    ("(let ((x '(a))) (eq? x x))", _.bool(true)),
    ("(let ((x (make-vector 0 1))) (eq? x x))", _.bool(true)),
    ("(let ((p (lambda (x) x))) (eq? p p))", _.bool(true)),
    ("(equal? 'a 'a)", _.bool(true)),
    ("(equal? '(a) '(a))", _.bool(true)),
    ("(equal? '(a (b) c) '(a (b) c))", _.bool(true)),
    ("(equal? \"abc\" \"abc\")", _.bool(true)),
    ("(equal? 2 2)", _.bool(true)),
    ("(equal? 1 2)", _.bool(false)),
    ("(equal? #\\a #\\b)", _.bool(false)),
    ("(equal? '(a b c) '(a b))", _.bool(false)),
    ("(equal? (cons 'a (cons 'b (cons 'c '()))) '(a b c))", _.bool(true)),
    ("(equal? '(a b c) '(a c b))", _.bool(false)),
    ("(real? 3)", _.bool(true)),
    ("(real? 1.5)", _.bool(true)),
    ("(integer? 0)", _.bool(true)),
    ("(integer? '())", _.bool(false)),
    ("(number? 0)", _.bool(true)),
    ("(number? -1)", _.bool(true)),
    ("(number? 0.5)", _.bool(true)),
    ("(number? '())", _.bool(false)),
    ("(odd? 0)", _.bool(false)),
    ("(odd? 1)", _.bool(true)),
    ("(odd? 101)", _.bool(true)),
    ("(max 3 4)", _.number(4)),
    ("(max 3.9 4)", _.number(4)),
    ("(max 1)", _.number(1)),
    ("(max 1 2 3 4 5 4 3 2 1)", _.number(5)),
    ("(min 3 4)", _.number(3)),
    ("(min 3 4.9)", _.number(3)),
    ("(min 1)", _.number(1)),
    ("(min 5 4 3 2 1 2 3 4 5)", _.number(1)),
    ("(+ 3 4)", _.number(7)),
    ("(+ 3)", _.number(3)),
    ("(+)", _.number(0)),
    ("(* 3 4)", _.number(12)),
    ("(* 4)", _.number(4)),
    ("(*)", _.number(1)),
    ("(- 3 4)", _.number(-1)),
    ("(- 3 4 5)", _.number(-6)),
    ("(- 3)", _.number(-3)),
    ("(/ 4 2)", _.number(2)),
    ("(/ 1 2)", _.real(0.5)),
    ("(/ 1.0 1)", _.real(1.0)),
    ("(/ 1 1.0)", _.real(1.0)),
    ("(/ 4 2.0)", _.real(2.0)),
    ("(< 1 2)", _.bool(true)),
    ("(< 2 1)", _.bool(false)),
    ("(< 1 1)", _.bool(false)),
    ("(< 2.0 2.1)", _.bool(true)),
    ("(< 2.1 2.0)", _.bool(false)),
    ("(<= 1 2)", _.bool(true)),
    ("(<= 2 1)", _.bool(false)),
    ("(<= 1 1)", _.bool(true)),
    ("(<= 2.0 2.1)", _.bool(true)),
    ("(<= 2.1 2.0)", _.bool(false)),
    ("(> 1 2)", _.bool(false)),
    ("(> 2 1)", _.bool(true)),
    ("(> 1 1)", _.bool(false)),
    ("(> 2.0 2.1)", _.bool(false)),
    ("(> 2.1 2.0)", _.bool(true)),
    ("(>= 1 2)", _.bool(false)),
    ("(>= 2 1)", _.bool(true)),
    ("(>= 1 1)", _.bool(true)),
    ("(>= 2.0 2.1)", _.bool(false)),
    ("(>= 2.1 2.0)", _.bool(true)),
    ("(= 1 1)", _.bool(true)),
    ("(= 2 1)", _.bool(false)),
    ("(abs -7)", _.number(7)),
    ("(abs 7)", _.number(7)),
    ("(abs 0)", _.number(0)),
    ("(modulo 13 4)", _.number(1)),
    ("(modulo -13 4)", _.number(3)),
    ("(modulo 13 -4)", _.number(-3)),
    ("(modulo -13 -4)", _.number(-1)),
    ("(quotient 3 5)", _.number(0)),
    ("(quotient 4 2)", _.number(2)),
    ("(quotient -6 2)", _.number(-3)),
    ("(remainder 13 4)", _.number(1)),
    ("(remainder -13 4)", _.number(-1)),
    ("(remainder 13 -4)", _.number(1)),
    ("(remainder -13 -4)", _.number(-1)),
    ("(expt 5 2)", _.number(25)),
    ("(expt 1 0)", _.number(1)),
    ("(expt 0 0)", _.number(1)),
    ("(ceiling -4.3)", _.real(-4.0)),
    ("(ceiling 3.5)", _.real(4.0)),
    ("(floor -4.3)", _.real(-450)),
    ("(floor 3.5)", _.real(3.0)),
    ("(floor 7)", _.number(7)),
    ("(sin 0)", _.real(0.0)),
    ("(asin 0)", _.real(0.0)),
    ("(cos 0)", _.real(1.0)),
    ("(acos 1)", _.real(0.0)),
    ("(tan 0)", _.real(0)),
    ("(< (- (tan 4) (/ (sin 4) (cos 4))) 0.0001)", _.bool(true)),
    ("(atan 0)", _.real(0.0)),
    ("(sqrt 0)", _.number(0)),
    ("(sqrt 4)", _.number(2)),
    ("(sqrt 16)", _.number(4)),
    ("(sqrt 4.0)", _.real(2.0)),
    ("(log 1)", _.real(0.0)),
    ("(negative? 0)", _.bool(false)),
    ("(negative? -1)", _.bool(true)),
    ("(negative? 1)", _.bool(false)),
    ("(positive? 0)", _.bool(false)),
    ("(positive? -1)", _.bool(false)),
    ("(positive? 1)", _.bool(true)),
    ("(zero? 0)", _.bool(true)),
    ("(zero? 1)", _.bool(false)),
    ("(zero? -1)", _.bool(false)),
    ("(even? 0)", _.bool(true)),
    ("(even? 1)", _.bool(false)),
    ("(even? -1)", _.bool(false)),
    ("(even? -2)", _.bool(true)),
    ("(exact->inexact 5)", _.real(5.0)),
    ("(exact->inexact 0)", _.real(0.0)),
    ("(inexact->exact 5.0)", _.number(5)),
    ("(inexact->exact 0.000)", _.number(5)),
    ("(number->string 0)", _.string("0")),
    ("(number->string .5)", _.string("0.5")),
    ("(number->string -123.456)", _.string("-123.456")),
    ("(not #t)", _.bool(false)),
    ("(not 3)", _.bool(false)),
    ("(not (cons 3 '()))", _.bool(false)),
    ("(not #f)", _.bool(true)),
    ("(not '())", _.bool(false)),
    ("(not (list))", _.bool(false)),
    ("(not 'nil)", _.bool(false)),
    ("(boolean? #f)", _.bool(true)),
    ("(boolean? 0)", _.bool(false)),
    ("(boolean? '())", _.bool(false)),
    ("(pair? (cons 'a 'b))", _.bool(true)),
    ("(pair? '(a b c))", _.bool(true)),
    ("(pair? '())", _.bool(false)),
    ("(equal? (cons 'a '()) '(a))", _.bool(true)),
    ("(equal? (cons '(a) '(b c d)) '((a) b c d))", _.bool(true)),
    ("(equal? (cons \"a\" '(b c)) '(\"a\" b c))", _.bool(true)),
    ("(equal? (car '(a b c)) 'a)", _.bool(true)),
    ("(equal? (car '((a) b c d)) '(a))", _.bool(true)),
    ("(equal? (car (cons 1 2)) 1)", _.bool(true)),
    ("(equal? (cdr '((a) b c d)) '(b c d))", _.bool(true)),
    ("(equal? (cdr (cons 1 2)) 2)", _.bool(true)),
    ("(null? '())", _.bool(true)),
    ("(null? (list))", _.bool(true)),
    ("(null? '(1 2 3))", _.bool(false)),
    ("(list? '(a b c))", _.bool(true)),
    ("(list? '((a b) c d))", _.bool(true)),
    ("(list? '())", _.bool(true)),
    ("(list? (cons 'a 'b))", _.bool(false)),
    ("(list? 'a)", _.bool(false)),
    ("(let ((x '(a))) (set-cdr! x x) (list? x))", _.bool(false)),
    ("(let ((x (cons 1 2))) (set-car! x 3) (and (= (car x) 3) (= (cdr x) 2)))", _.bool(true)),
    ("(let ((x (cons 1 2))) (set-cdr! x 3) (and (= (car x) 1) (= (cdr x) 3)))", _.bool(true)),
    ("(equal? (list 'a (+ 3 4) 'c) '(a 7 c))", _.bool(true)),
    ("(list)", _.nil),
    ("(length '(a b c))", _.number(3)),
    ("(length '(a (b) (c d e)))", _.number(3)),
    ("(length '())", _.number(0)),
    ("(list-ref '(a b c) 0)", _.symbol("a")),
    ("(list-ref '(a b c d) 2)", _.symbol("c")),
    ("(list-ref '(a b c d) (inexact->exact (round 1.8)))", _.symbol("c")),
    ("(equal? (memq 'a '(a b c)) '(a b c))", _.bool(true)),
    ("(equal? (memq 'b '(a b c)) '(b c))", _.bool(true)),
    ("(memq 'a '(b c d))", _.bool(false)),
    ("(memq (list 'a) '(b (a) c))", _.bool(false)),
    ("(equal? (member (list 'a) '(b (a) c)) '((a) c))", _.bool(true)),
    ("(member 'd '(a b c))", _.bool(false)),
    ("(equal? (assq 'a '((a 1) (b 2) (c 3))) '(a 1))", _.bool(true)),
    ("(equal? (assq 'b '((a 1) (b 2) (c 3))) '(b 2))", _.bool(true)),
    ("(equal? (assq 'c '((a 1) (b 2) (c 3))) '(c 3))", _.bool(true)),
    ("(assq 'd '((a 1) (b 2) (c 3)))", _.bool(false)),
    ("(assq (list 'a) '(((a)) ((b)) ((c))))", _.bool(false)),
    ("(equal? (assoc (list 'a) '(((a)) ((b)) ((c)))) '((a)))", _.bool(true)),
    ("(symbol? 'foo)", _.bool(true)),
    ("(symbol? (car '(a b)))", _.bool(true)),
    ("(symbol? \"bar\")", _.bool(false)),
    ("(symbol? 'nil)", _.bool(true)),
    ("(symbol? '())", _.bool(false)),
    ("(symbol? #f)", _.bool(false)),
    ("(symbol->string 'flying-fish)", _.string("flying-fish")),
    ("(string->symbol \"flying-fish\")", _.symbol("flying-fish")),
    ("(char? #\\a)", _.bool(true)),
    ("(char? 0)", _.bool(false)),
    ("(char? '())", _.bool(false)),
    ("(string-append \"foo\" \"bar\")", _.string("foobar")),
    ("(string-length \"foobar\")", _.number(6)),
    ("(string? 'foo)", _.bool(false)),
    ("(string? 1)", _.bool(false)),
    ("(string? \"\")", _.bool(true)),
    ("(string? \"foo\")", _.bool(true)),
    ("(string<? \"foo\" \"bar\")", _.bool(false)),
    ("(string<? \"bar\" \"foo\")", _.bool(true)),
    ("(string<? \"foo\" \"foo\")", _.bool(false)),
    ("(string<? \"f\" \"foo\")", _.bool(true)),
    ("(let ((vec (vector 'a 'b 'c))) (and (equal? (vector-ref vec 0) 'a) (equal? (vector-ref vec 1) 'b) (equal? (vector-ref vec 2) 'c)))", _.bool(true)),
    ("(let ((vec (vector 0 '(2 2 2 2) \"Anna\"))) (vector-set! vec 1 '(\"Sue\" \"Sue\")) (and (equal? (vector-ref vec 0) 0) (equal? (vector-ref vec 1) '(\"Sue\" \"Sue\")) (equal? (vector-ref vec 2) \"Anna\")))", _.bool(true)),
    ("(vector? (vector 'a 'b 'c))", _.bool(true)),
    ("(vector? 'a)", _.bool(false)),
    ("(vector-length (vector))", _.number(0)),
    ("(vector-length (vector 0 1 0))", _.number(3)),
  )

}
