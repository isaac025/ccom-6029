#lang plait

;; scan errors only
(print-only-errors #true)

;; How we represent arithmethic
(define-type Exp
  [num (n : Number)]
  [plus (left : Exp)(right : Exp)])

;; How we evaluate arithmetic
(calc : (Exp -> Number))

(define (calc e)
  (type-case Exp e
    [(num n) n]
    [(plus l r) (+ (calc l) (calc r))]))

;; Tests our calculator
(test (calc (num 1)) 1)
(test (calc (num 2.3)) 2.3)
(test (calc (plus (num 1) (num 2))) 3)
(test (calc (plus (plus (num 1) (num 2))
                  (num 3)))
      6)
(test (calc (plus (num 1)
                  (plus (num 2) (num 3))))
      6)
(test (calc (plus (num 1)
                  (plus (plus (num 2)
                              (num 3))
                        (num 4))))
      10)

;; Subtler tests
(test (calc (plus (num 0.1) (num 0.2))) 0.3) ; works!

(test (calc (plus (num 0.1) (num 0.2))) 1/3) ; FAILS (we adopted the language's semantics!)