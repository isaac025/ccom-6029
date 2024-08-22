#lang plait

;; How we represent arithmethic
(define-type Exp
  [num (n : Number)]
  [plus (left : Exp)(right : Exp)])

;; How we evaluate arithmetic
(calc : (Exp -> Number))

(define (calc e)
  (type-case Exp e
    [(num n) n]
    [(plus l r) (+ (calc l) (calc r))]))

;; Our parser takes S-Expressions and produces Expressions
(parse : (S-Exp -> Exp))

(define (parse s)
  (cond
    [(s-exp-number? s)
     (num (s-exp->number s))]
    [(s-exp-list? s)
     (let ([l (s-exp->list s)])
       (if (symbol=? '+
                     (s-exp->symbol (first l)))
           (plus (parse (second l))
                 (parse (third l)))
           (error 'parse "list not an addition")))]))

;; test for parser
(test (parse `1) (num 1))
(test (parse `2.3) (num 2.3))
(test (parse `{+ 1 2}) (plus (num 1) (num 2)))
(test (parse `{+ 1
                 {+ {+ 2 3}
                    4}})
      (plus (num 1)
            (plus (plus (num 2)
                        (num 3))
                  (num 4))))

(test/exn (parse `{1 + 2}) "{+ 1 2}")

;; compose calc and parse to run our program
(run : (S-Exp -> Number))

(define (run s)
  (calc (parse s)))

;; test run our evaluator
(test (run `1) 1)
(test (run `2.3) 2.3)
(test (run `{+ 1 2}) 3)
(test (run `{+ {+ 1 2} 3})
      6)
(test (run `{+ 1 {+ 2 3}})
      6)
(test (run `{+ 1 {+ {+ 2 3} 4}})
      10)