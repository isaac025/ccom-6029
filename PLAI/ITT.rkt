#lang plait

;; How we define Expressions
;; Exercise add division
(define-type BinOp
  [plus] [++] [div])

(define-type Exp
  [binE (operator : BinOp)
        (left : Exp)
        (right : Exp)]
  [numE (value : Number)]
  [strE (value : String)])

;; How we define Values
(define-type Value
  [numV (the-number : Number)]
  [strV (the-string : String)])

;; How we define types
(define-type Type [numT] [strT])

;; How we evaluate
(calc : (Exp -> Value))

(define (calc e)
  (type-case Exp e
    [(binE o l r)
     (type-case BinOp o
       [(plus) (add (calc l) (calc r))]
       [(++) (concat (calc l) (calc r))]
       [(div) (divide (calc l) (calc r))])]
    [(numE v) (numV v)]
    [(strE s) (strV s)]))

;; Absraction/Helper to handle addition
(define (add v1 v2)
  (type-case Value v1
    [(numV n1)
     (type-case Value v2
       [(numV n2) (numV (+ n1 n2))]
       [else (error '+ "expects RHS to be a number")])]
    [else (error '+ "expects LHS to be a number")]))

;; Absraction/Helper to handle division
(define (divide v1 v2)
  (type-case Value v1
    [(numV n1)
     (type-case Value v2
       [(numV n2) (numV (/ n1 n2))]
       [else (error '+ "expects RHS to be a number")])]
    [else (error '+ "expects LHS to be a number")]))

;; Absraction/Helper to handle concatanation
(define (concat v1 v2)
  (type-case Value v1
    [(strV s1)
     (type-case Value v2
       [(strV s2) (strV (string-append s1 s2))]
       [else (error '+ "expects RHS to be a string")])]
    [else (error '+ "expects LHS to be a string")]))

(test (calc (binE (plus) (numE 5) (numE 6))) (numV 11))

(test (calc (binE (++) (strE "hello, ") (strE "world!"))) (strV "hello, world!"))

;; How we implement a type checker
(tc : (Exp -> Type))

(define (tc e)
  (type-case Exp e
    [(binE o l r)
     (type-case BinOp o
       [(plus) (if (and (numT? (tc l)) (numT? (tc r)))
                   (numT)
                   (error 'tc "not both numbers"))]
       [(++) (if (and (strT? (tc l)) (strT? (tc r)))
                 (strT)
                 (error 'tc "not both strings"))]
       [(div) (if (and (numT? (tc l)) (numT? (tc r)))
                  (numT)
                  (error 'tc "not both numbers"))])]
    [(numE v) (numT)]
    [(strE s) (strT)]))

;; Tests that work
(test (tc (binE (plus) (numE 5) (numE 6))) (numT))
(test (tc (binE (++) (strE "hello") (strE "world"))) (strT))

;; Tests that fail
(test/exn (tc (binE (++) (numE 5) (strE "bad"))) "strings")
(test/exn (tc (binE (plus) (numE 6) (strE "bad"))) "numbers")
