#lang racket

;; Strict if as a function. PROBLEM - eager evaluation
(define (strict-ifF C T E)
  (if (boolean? C)
      (if C T E)
      (error 'strict-if "expected a boolean")))

;; Strict if as a macro (syntax definition)
(define-syntax strict-if
  (syntax-rules ()
    [(strict-if C T E)
     (if (boolean? C)
         (if C T E)
         (error 'strict-if "expected a boolean"))]))

(strict-if true 1 (/ 1 0))

;; My-Let that is a sort of lambda
(define-syntax my-let1
  (syntax-rules ()
    [(my-let1 (var val) body)
     ((lambda (var) body) val)]))

(my-let1 (x 3) (+ x x))

;; Incorrect My-Let version
#|
(define-syntax my-let2
  (syntax-rules ()
    [(my-let1 (var val) body)
     ((lambda (var) val) body)]))

(my-let2 (x 3) (+ x x)) ; x is unbound
|#

;; My-Let2 to handle more than 1 binding (actually ... is 0 or more)
(define-syntax my-let2
  (syntax-rules ()
    [(my-let2 ([var val] ...) body)
     ((lambda (var ...) body) val ...)]))

(my-let2 ([x 3] [y 4]) (+ x y))

;; My-Cond shows that if zero instances occurr we fall to an error
(define-syntax my-cond
  (syntax-rules ()
    [(my-cond) (error 'my-cond "should not get here")]
    [(my-cond [q0 a0] [q1 a1] ...)
     (if q0
         a0
         (my-cond [q1 a1] ...))]))

(define (sign  n)
  (my-cond
   [(< n 0) "negative"]
   [(= n 0) "zero"]
   [(> n 0) "positive"]))