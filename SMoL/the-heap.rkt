#lang stacker/smol/state

(defvar x (mvec 1 0 2))
(vec-set! x 1 x)
(vlen x)

(defvar v (mvec 1 2 3))
(defvar vv (mpair v v))
(vec-set! (right vv) 0 6)
(left vv)
