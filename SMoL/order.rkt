#lang stacker/smol/fun

(deffun (addy x)
  (+ x y))
(defvar s (addy 1))
(defvar y 2)
s
