#lang plait

;; How we represent expressions
(define-type Exp
  [numE (n : Number)]
  [boolE (b : Boolean)]
  [plusE (left : Exp)(right : Exp)]
  [cndE (test : Exp) (then : Exp) (else : Exp)])

;; How we represent values
(define-type Value
  [numV (the-number : Number)]
  [boolV (the-boolean : Boolean)])

;; How we evaluate: from expressions to values
(calc : (Exp -> Value))

(define (calc e)
  (type-case Exp e
    [(numE n) (numV n)] ; We cannot use numV here because calc expects an expression.
    [(boolE b) (boolV b)]
    [(plusE l r) (add (calc l) (calc r))]
    [(cndE c t e) (if (boolean-decision (calc c))
                      (calc t)
                      (calc e))]))

;; Absraction/Helper to handle addition
(define (add v1 v2)
  (type-case Value v1
    [(numV n1)
     (type-case Value v2
       [(numV n2) (numV (+ n1 n2))]
       [else (error '+ "expects RHS to be a number")])]
    [else (error '+ "expects LHS to be a number")]))

;; Abstraction/Helper for conditional, strict for boolean
(define (boolean-decision v)
  (type-case Value v
    [(boolV b) b]
    [else (error 'if "expects conditional to evaluate to a boolean")]))
                          
;; Test for calc
(test (calc (numE 1)) (numV 1))
(test (calc (numE 2.3)) (numV 2.3))
(test (calc (plusE (numE 1) (numE 2))) (numV 3))
(test (calc (plusE (plusE (numE 1) (numE 2))
                  (numE 3)))
      (numV 6))


(test (calc (plusE (numE 1)
                  (plusE (numE 2) (numE 3))))
      (numV 6))
(test (calc (plusE (numE 1)
                  (plusE (plusE (numE 2)
                                (numE 3))
                         (numE 4))))
      (numV 10))

;; Subtler tests
(test/exn (calc (plusE (numE 0.1) (boolE #false))) "RHS")

;; Boolean test
(test (calc (boolE #false)) (boolV #false))
(test (calc (boolE #true)) (boolV #true))

(test (calc (cndE (boolE #true)
                  (numE 1)
                  (numE 2)))
            (numV 1))

(test (calc (cndE (boolE #false)
                  (numE 1)
                  (plusE (numE 1) (numE 2))))
            (numV 3))

(test/exn (calc (cndE (numE 1)
                      (boolE #false)
                      (numE 0)))
          "conditional")