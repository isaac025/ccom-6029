#lang racket

(define (fact n)
  (if (= n 0)
      1
      (* n (fact (- n 1)))))


(define my-list (list 1 2 3))

(define (my-length l)
  (cond [(null? l) 0]
        (else (+ 1 (my-length (rest l))))))

;; una suma
'(+ 3 4)

;; otra expression
'(+ (* 2 n) 23)

(define (depth tree)
  (cond [(not (list? tree)) 0]
        [(or (null? tree) (= (length tree) 1)) 0]
        (else (+ 1 (max (depth (second tree))
                        (depth (third tree)))))))


;; Asignacion: Funcion que devuelva el profundo de un tree
(define (depth2 tree)
  (if (or (not (list? tree)) (null? tree))
      0
      (let ([left (first tree)]
            [right (rest tree)])
        (+ 1 (max (depth2 left)
                  (depth2 right))))))