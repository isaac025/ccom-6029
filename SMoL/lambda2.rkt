#lang stacker/smol/hof

(defvar x 1)
(deffun (make-f)
  (deffun (addx y)
    (+ x y))
  addx)
(defvar f (make-f))
(set! x 2)
(f x)

(deffun (make-counter count)
  (deffun (counter)
    (set! count (+ count 1))
    count)
  counter)
(defvar f (make-counter 0))
(defvar g (make-counter 0))

(f)
(f)
(g)
