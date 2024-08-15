#lang stacker/smol/state

(defvar v (mvec 1 2 3 4))
(defvar vv (mvec v v))
(vec-set! (vec-ref vv 1) 0 100)
vv
