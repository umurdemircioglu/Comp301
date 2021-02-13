#lang racket

;;;;;;;;;;;;;;;;;;;;;;; TASK 4 ;;;;;;;;;;;;;;;;;;;;;;;;

(define fibonacci
  (lambda (n)
    (fibonacci-c n (lambda (x) x))))
  
(define fibonacci-c
  (lambda (n cont)
    (cond
      ((zero? n) (cont 0))
      ((= n 1) (cont 1))
      (else
       (define a (fibonacci-c (- n 1) (lambda (x) x)))
       (define b (fibonacci-c (- n 2) (lambda (x) x)))
       (cont (+ a b))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
; i:    1  2  3  4  5  6  7  8  9  10 11 ...
; f(i): 1  1  2  3  5  8  13 21 34 55 ...

;; Tests
(display (fibonacci 4)) ; should output 3
(display  "\n")
(display (fibonacci 7)) ; should output 13
(display  "\n")
(display (fibonacci 8)) ; should output 21
(display  "\n")