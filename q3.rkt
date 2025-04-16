#lang eopl
(require "utils.rkt")

  (define report-no-binding-found
    (lambda (search-var)
      (eopl:error 'apply-env "No binding for ~s" search-var)))

;empty-env: () -> Env
 (define (empty-env)
   (lambda (var maybe-val)
     (if (cdr maybe-val)
         #f
         ((report-no-binding-found var)))))


;extend-env: var x val x Env -> Env
(define (extend-env var val env)
  (lambda (runtime-var maybe-val)
    (if (not (eq? var runtime-var))
        ;if we dont match on var go to the parent env
        (env runtime-var maybe-val)

        ;check which subfunction we need
        (if (not (cdr maybe-val))
            val ;regular env extraction

            ;check if this (var val) pair is the binding we need
            (if (equal? (car maybe-val) val)
                #t
                (env runtime-var maybe-val))) ;if it isnt we ask the parent env
        )))
            
            
;apply-env: Env x var -> val
(define (apply-env env var)
  (env var (cons '()  #f)))

;xy-binding: Env x var x val -> bool
(define (xy-binding env var val)
  (env var (cons val #t)))

;testing

;baseline functitonality
(equal?? 2 (apply-env (extend-env 'x 2 (empty-env)) 'x))

(equal?? 2
         (apply-env
             
             (extend-env 'y 21
             (extend-env 'x 2
             (extend-env 'x 1 (empty-env))))

           'x)
)

;new stuff
(equal?? #f (xy-binding (empty-env) 'x 1))
(equal?? #f (xy-binding (extend-env 'x 2 (empty-env)) 'x 1))
(equal?? #t (xy-binding (extend-env 'x 2 (empty-env)) 'x 2))


(equal?? #t
         (xy-binding
             
             (extend-env 'y 21
             (extend-env 'x 2
             (extend-env 'x 1 (empty-env))))

           'x 1)
)
