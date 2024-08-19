#lang plait
(define-type Exp
  [num (n : Number)]
  [plus (left : Exp)(right : Exp)])
