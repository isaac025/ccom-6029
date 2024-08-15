#lang stacker/smol/hof

(let ([f (lambda (n)
           (or (equal? n 0)
               (f (- n 1))))])
  (f 3))

(defvar x 1)
(defvar y 2)

(letrec ([x (+ y 1)]
         [y (+ x 1)])
  (mvec x y))
