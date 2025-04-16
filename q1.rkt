#lang eopl
  (require "utils.rkt")

;2d vector
(define-datatype vector vector?
  (vec
   (x number?)
   (y number?)))

;simple constructors

;create-vec: float x float -> vec
;(x,y) -> vec(x,y)
(define (create-vec x y)
  (vec x y))

;zero-vec: () -> vec
; () -> vec(0,0)
(define (zero-vec)
  (vec 0 0))

;(display (zero-vec))

;all the rest are observers

;helper to get the length of a vector
(define (vec-length v)
  (cases vector v
    [vec (x y)
     (sqrt (+ (* x x) (* y y)))]))

;helper to get the angle of a vector
(define (vec-angle v)
  (cases vector v
    [vec (x y)
     (atan y x)]))

;PRINTS the vec in polar form
;polar-vec: vec -> !print
;print atan(y/x) and sqrt(x*x+y*y)
(define (polar-vec v)
  (display "angle: ")
  (display (vec-angle v))
  (display " length")
  (display (vec-length v)))

;(polar-vec (create-vec 1 1))

;add v2 and v1
;add-vec: vec x vec -> vec
;vec(x1,y1) vec(x2,y2) -> vec(x1+x2,y1+y2) 
(define (add-vec v1 v2)
  (let ([x1 (cases vector v1
              [vec (x y) x])]
        [y1 (cases vector v1
              [vec (x y) y])]
        [x2 (cases vector v2
              [vec (x y) x])]
        [y2 (cases vector v2
              [vec (x y) y])])
    (vec (+ x1 x2) (+ y1 y2))))


;(display (add-vec (vec 1 1) (vec 1 -1)))
(equal?? (vec 2 0) (add-vec (vec 1 1) (vec 1 -1)))

;subtract v2 from v1
;sub-vec vec x vec -> vec
;vec(x1,y1) vec(x2,y2) -> vec(x1-x2,y1-y2) 
(define (sub-vec v1 v2)
  (let ([x1 (cases vector v1
              [vec (x y) x])]
        [y1 (cases vector v1
              [vec (x y) y])]
        [x2 (cases vector v2
              [vec (x y) x])]
        [y2 (cases vector v2
              [vec (x y) y])])
    (vec (- x1 x2) (- y1 y2))))

(equal?? (vec 1 1) (sub-vec (vec 2 0) (vec 1 -1)))

;returns the quadrent in the cartesian plane the vector is at
;quad-vec: vec -> int
;v -> quadrent(v)
(define (quad-vec v)
  (cases vector v
    [vec (x y)
         (if (<= x 0)
             (if (<= y 0) 1 4)
             (if (<= y 0) 2 3))]))

(equal?? 1 (quad-vec (zero-vec)))

;(vec-length (sub-vec (vec 1 1) (vec 2 2)))

;checks if 2 vectors are exact negations of 1 another
;is-neg?: vec x vec -> bool

;vec(x,y) vec(-x,-y) -> true
; otherwise false
(define (is-neg? v1 v2)
  (equal? (zero-vec) (add-vec v1 v2))
)

(equal?? #t (is-neg? (vec 1 1) (vec -1 -1)))

;(equal?? #t (is-neg? (vec 1 1) (vec -1 -1)))