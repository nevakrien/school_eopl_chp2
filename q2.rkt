#lang eopl
(require "utils.rkt")

(define-datatype prefix-exp prefix-exp?
  (const-exp
     (num integer?))
  (diff-exp
    (operand1 prefix-exp?)
    (operand2 prefix-exp?))
 )

(define (minus? x) (eq? x '-))

(define-datatype token token?
  (diff (_ minus?))
  (leaf (num integer?))
)

(define (lex x)
  (cond
    [(minus? x) (diff x)]
    [(integer? x) (leaf x)]))


;parse-inner: list<Tokens> -> (prefix-exp,tokens)
;consumes tokens untill a 
(define (parse-inner tokens)
  (cases token (car tokens)
    [diff (_)
       (letrec (
         [left (parse-inner (cdr tokens))]
         [right (parse-inner (cdr left))])

         (cons
           (diff-exp (car left) (car right))
           (cdr right))
             
         )]
    [leaf (num) (cons (const-exp num) (cdr tokens))]
 ))

;prefix-parse: list<Tokens> -> prefix-exp
;parses tokens into prefix-exp
(define (prefix-parse tokens)
  (let ([result (parse-inner (map lex tokens))])
    (if (null? (cdr result))
        (car result)
        (eopl:error 'prefix-parse "Extra tokens after expression: ~s" (cdr result)))))


;tests
;(display (prefix-parse (list '- 3 '- 5 2)))

;===crashes===
;extra token
;(prefix-parse (list '- 3 '- 5 2 3))

;lex error
;(prefix-parse (list '- 3 '+ 5 2 3))

;===green tests===
(equal??
 (prefix-parse '( - 3 - 5 2))
 (diff-exp
   (const-exp 3)
   (diff-exp
     (const-exp 5)
     (const-exp 2))))


(equal??
 (prefix-parse '( - 3 - - 5 3 2))
 (diff-exp
   (const-exp 3)
   (diff-exp
    (diff-exp
     (const-exp 5)
     (const-exp 3))
     (const-exp 2))))
