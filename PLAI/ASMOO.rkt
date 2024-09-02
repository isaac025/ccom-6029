;; Exercise spot where type checker will be problematic (use #lang plait)
#lang racket
(require [only-in plait test print-only-errors])

;; Object base (match on lambda)
(define o
  (lambda (m)
    (case m
      [(add1) (lambda (x) (+ x 1))]
      [(sub1) (lambda (x) (- x 1))])))

(test ((o 'add1) 5) 6)

;; Dispatch an on object with a message
(define (msg o m . a)
  (apply (o m) a))

(test (msg o 'add1 5) 6)

(test (msg o (first '(add1)) 5) 6)

;; Object constructor
(define (o-constr x)
  (lambda (m)
    (case m
      [(addX) (lambda (y) (+ x y))])))

(test (msg (o-constr 5) 'addX 3) 8)
(test (msg (o-constr 2) 'addX 3) 5)

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

(test (let ([o (mk-o-state 5)])
        (begin (msg o 'inc)
               (msg o 'inc)
               (msg o 'dec)
               (msg o 'get)))
        6)
;; Mutating one object does not affect the other
(test (let ([o1 (mk-o-state 3)]
            [o2 (mk-o-state 3)])
        (begin (msg o1 'inc)
               (msg o1 'inc)
               (+ (msg o1 'get)
                  (msg o2 'get))))
      (+ 5 3))

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

(test (let ([o (mk-o-static 1000)])
        (msg o 'count))
      1)
(test (let ([o (mk-o-static 0)])
        (msg o 'count))
        2)

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

(test (msg/self o-self-no! 'first 5) 7)

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

(test (msg a-tree 'sum) (+ 10 5 15 6))
