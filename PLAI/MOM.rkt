#lang racket

;; One-armed if (useful for erroneous conditions) i.e. unless
(define-syntax unless
  (syntax-rules ()
    [(_ cond body ...)
     (if (not cond)
         (begin
           body
           ...)
         (void))]))

;; happens
(unless false
  (println 1)
  (println 2))

;; doesn't happen
(unless true
  (println 6)
  (println 7))

;; works as expected thanks to macros
(let ([not (lambda (v) v)])
  (unless false
    (println "this")
    (println "happens")))

;; does not works as expected (macros do not expand this way!)
(let ([not (lambda (v) v)])
  (if (not false)
      (begin
        (println "this")
        (println "doesn't"))
      (void)))

;; Two-armed or (naive implementation)
(define-syntax or-2-naive
  (syntax-rules ()
    [(_ e1 e2)
     (if e1
         true
         e2)]))

;; Works well
(or-2-naive true false)
(or-2-naive false false)
(or-2-naive false true)

;; Combined with a function like member, doesn't work as expected.
(or-2-naive (member 'y '(x y z)) "not found")

;; Two-armed or better
(define-syntax or-2-subtle-peril
  (syntax-rules ()
    [(_ e1 e2)
     (if e1
         e1
         e2)]))

;; Works now
(or-2-subtle-peril (member 'y '(x y z)) "not found")

;; But has a subtle peril
(println "sublte peril")
(or-2-subtle-peril (print "hello") "not found")
(println "")
;; Two-armed or correct
(define-syntax or-2
  (syntax-rules ()
    [(_ e1 e2)
     (let ([v e1])
       (if v v e2))]))

;; Now works as expected
(println "works correctly")
(or-2 (print "hello") "not found")

;; N-ary or (naively implemented)
(define-syntax orN-naive
  (syntax-rules ()
    [(_ e1 e2 ...)
     (let ([v e1])
       (if v v (orN e2 ...)))]))

;; Error (after e2 above is 0 or more)
#|(let ([v true])
  (orN-naive false v))
|#

;; N-ary or correct
(define-syntax orN
  (syntax-rules ()
    [(_) false]
    [(_ e1 e2 ...)
     (let ([v e1])
       (if v v (orN e2 ...)))]))

;; Works as expected
(println "printing n-ary or")
(let ([v true])
  (orN false v))

;; Exercise: N-ary or with just one
(define-syntax orN-ex
  (syntax-rules ()
    [(_ e ...)
     (if (null? e ...)
         false
         (let ([v (first e ...)]
               [rst (rest e ...)])
           (if v v (orN-ex rst))))]))

;; Not sure if this is how it is
;; (orN-ex false false false true) ; WRONG!