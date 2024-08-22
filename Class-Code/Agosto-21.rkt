#lang racket

;; Contar variables de un arbol
(define (count-var tree)
  (if (null? tree)
      0
      (let ([cur (first tree)]
            [rst (rest tree)])
            (cond [(list? cur) (count-var cur)]
                  [(and (not-math-sym cur) (not (number? cur))) (+ 1 (count-var rst))]
                  (else (count-var rst))))))

(define (not-math-sym e)
  (cond [(eq? e '+) #f]
        [(eq? e '*) #f]
        [(eq? e '-) #f]
        [(eq? e '/) #f]
        (else #t)))