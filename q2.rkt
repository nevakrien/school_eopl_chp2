#lang eopl
(require "utils.rkt")

(define-datatype prefix-exp prefix-exp?
  (const-exp
     (num integer?))
  (diff-exp
    (operand1 prefix-exp?)
    (operand2 prefix-exp?))
 )

(define (equal-helper x)
  (lambda (y) (eq? y x)))

(define-datatype token token?
  (minus (equal-helper '-))
  (num (num integer?))
  (open (equal-helper '|(|))
  (close (equal-helper '|)|)))

(define-datatype input valid-lex?
  (tokens (list-of token?)))


;(display (cdr '( a b c)))
`a