#lang plait

(define-type Exp
  [num (n : Number)]
  [plus (left : Exp) (right : Exp)])

(define p1 (plus (num 2) (num 3)))

(define p2 (plus (num 3) (num 4)))

(define p3 (plus (num 2) p1))

(define (eval e)
  (type-case Exp e
    [(num n) n]
    [(plus l r) (+ (eval l) (eval r))]))