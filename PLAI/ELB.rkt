#lang plait

;; How we represent expressions
(define-type Exp
  [numE (n : Number)]
  [boolE (b : Boolean)]
  [plusE (left : Exp)(right : Exp)]
  [cndE (test : Exp) (then : Exp) (else : Exp)]
  [varE (name : Symbol)]
  [let1E (var : Symbol)
         (value : Exp)
         (body : Exp)])

;; How we represent values
(define-type Value
  [numV (the-number : Number)]
  [boolV (the-boolean : Boolean)])

;; Hash table to represent environment
(define-type-alias Env (Hashof Symbol Value))
(define mt-env (hash empty)) ; empty environment

;; Look for a variable when we encounter one
(define (lookup (s : Symbol) (n : Env))
  (type-case (Optionof Value) (hash-ref n s)
    [(none) (error s "not bound")]
    [(some v) v]))

;; Extend the environment when a new binding is encountered
(extend : (Env Symbol Value -> Env))
(define (extend old-env new-name value)
  (hash-set old-env new-name value))

;; How we evaluate: an interpreter that has an environment to store variables
;; and produces values from expressions
(interp : (Exp Env -> Value))

(define (interp e env)
  (type-case Exp e
    [(numE n) (numV n)] ; We cannot use numV here because calc expects an expression.
    [(boolE b) (boolV b)]
    [(plusE l r) (add (interp l env) (interp r env))]
    [(cndE c t e) (if (boolean-decision (interp c env))
                      (interp t env)
                      (interp e env))]
    [(varE v) (lookup v env)]
    [(let1E var val body)
     (let ([new-env (extend env
                            var
                            (interp val env))])
           (interp body new-env))]))

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