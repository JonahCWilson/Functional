#lang racket

; Exercise 1.1
10
12
8
3
6
;No output
;No output
19
#false
4
16

; Exercise 1.2
; Define a procedure that takes 3 arguments and returns the sum of the squares
; of the larger two numbers
(define (square q) (* q q))


(define (sum-of-squares a b)
	(+ (square a) (square b)))

(define (sum-of-three a b c)
	(if (< a b) (sum-of-squares b (max a c))
		    (sum-of-squares a (max b c))))

(= (sum-of-three 10 20 30)
   (+ 400 900))



