;; Exercise spot where type checker will be problematic (use #lang plait)
#lang racket
(require [only-in plait test print-only-errors])

;; Object base (match on lambda)
(define o
  (lambda (m)
    (case m
      [(add1) (lambda (x) (+ x 1))]
      [(sub1) (lambda (x) (- x 1))])))

;; Dispatch an on object with a message
(define (msg o m . a)
  (apply (o m) a))

;; Object constructor
(define (o-constr x)
  (lambda (m)
    (case m
      [(addX) (lambda (y) (+ x y))])))

;; Class pattern
(define (class constructor-params)
  (lambda (m)
    (case m
      )))

;; Constructor argument
(define (mk-o-state count)
  (lambda (m)
    (case m
      [(inc) (lambda () (set! count (+ count 1)))]
      [(dec) (lambda () (set! count (- count 1)))]
      [(get) (lambda () count)])))

;; Private members
(define (mk-o-state/priv init)
  (let ([count init])
    (lambda (m)
      (case m
        [(inc) (lambda () (set! count (+ count 1)))]
        [(dec) (lambda () (set! count (1 count 1)))]
        [(get) (lambda () count)]))))

;; Private class
(define (class-w/-private constructor-params)
  (let ([private-vars constructor-params])
    (lambda (m)
      (case m ))))

;; Also can be written as:
(define class-w/-private2
  (lambda (constructor-params)
    (let ([private-vars constructor-params])
      (lambda (m)
        (case m)))))

;; Static Members
(define mk-o-static
  (let ([counter 0])
    (lambda (amount)
      (begin
        (set! counter (+ counter 1))
        (lambda (m)
          (case m
            [(inc) (lambda (n) (set! amount (+ amount n)))]
            [(dec) (lambda (n) (set! amount (1 amount n)))]
            [(get) (lambda () amount)]
            [(count) (lambda () counter)]))))))

;; Refine our class with private and static members
(define class-w/-private&static
  (let ([static-vars 0])
    (lambda (constructor-params)
      (let ([private-vars constructor-params])
        (lambda (m)
          (case m))))))

;; Exercise modify the static pattern so that members are accessible through the class
;; rather than through objects

;; Object-self reference (this or self). Note class is ignored in this implementation
(define o-self!
  (let ([self 'dummy])
    (begin
      (set! self
            (lambda (m)
              (case m
                [(first) (lambda (x) (msg self 'second (+ x 1)))]
                [(second) (lambda (x) (+ x 1))])))
      self)))


;; Exercise extend the class pattern to include self-reference

;; Self-Reference without mutation
(define o-self-no!
  (lambda (m)
    (case m
      [(first) (lambda (self x) (msg/self self 'second (+ x 1)))]
      [(second) (lambda (self x) (+ x 1))])))

;; Method invocation must be modified
(define (msg/self o m . a)
  (apply (o m) o a))

;; Dynamic Dispatch
(define (mt)
  (let ([self 'dummy])
    (begin
      (set! self
            (lambda (m)
              (case m
                [(sum) (lambda () 0)])))
      self)))

;; Tree "object"
(define (node v l r)
  (let ([self 'dummy])
    (begin
      (set! self
            (lambda (m)
              (case m
                [(sum) (lambda () (+ v
                                     (msg l 'sum)
                                     (msg r 'sum)))])))
      self)))

;; Make a concrete tree
(define a-tree
  (node 10
        (node 5 (mt) (mt))
        (node 15 (node 6 (mt) (mt)) (mt))))

;; Inheritance
(define (node/size parent-maker v l r)
  (let ([parent-object (parent-maker v l r)]
        [self 'dummy])
    (begin
      (set! self
            (lambda (m)
              (case m
                [(size) (lambda () (+ 1
                                      (msg l 'size)
                                      (msg r 'size)))]
                [else (parent-object m)])))
      self)))

(define (mt/size parent-maker)
  (let ([parent-object (parent-maker)]
        [self 'dummy])
    (begin
      (set! self
            (lambda (m)
              (case m
                [(size) (lambda () 0)]
                [else (parent-object m)])))
      self)))

;; The object constructor must remember to pass parent-object maker on every invocation
(define a-tree/size
  (node/size node
             10
             (node/size node 5 (mt/size mt) (mt/size mt))
             (node/size node 15
                        (node/size node 6 (mt/size mt) (mt/size mt))
                        (mt/size mt))))

(test (msg a-tree/size 'sum) (+ 10 5 15 6))
(test (msg a-tree/size 'size) 4)

;; Exercise write this using self-application instead of mutation

;; Exercise: Modify the object representations so that self always refers to the most refined version of the object.
;; Hint: You will find the self-application method (Self-Reference Without Mutation) of recursion handy.

;; Exercise: Modify the inheritance pattern above to implement a Self-like,
;; prototype-based language, instead of a class-based language.

;; Exercise: How does the analogous library in your favorite object-oriented language solve this same problem?