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

; Exercise 1.3

; With normal-order evaluation, the initial y, the call to (p), would be 
; evaluated prior to being placed as the else clause in the if statement.
; It would then enter a fatal recursive loop.  With applicative order application,
; the procedure would enter the if statement, evaluate the predicate, and
; return 0 without (p) ever being evaluated.

; Exercise 1.4
; Definitions for sqrt-iter and associated functions:
;(define (sqrt-iter guess x)
;	(if (good-enough? guess x)
;	    guess
;	    (sqrt-iter (improve guess x) x)))
;(define (average a b)
;	(/ (+ a b) 2))
;(define (improve guess x)
;	(average guess (/ x guess)))
;(define (good-enough? guess x)
;	(< (abs (- (square guess) x)) .001))
;(define (sqrt x)
;	(sqrt-iter 1 x))


(define (sqrt-iter prev guess x)
	(if (good-enough? prev guess) guess
		(sqrt-iter guess (improve guess x) x)))
(define (improve guess x)
	(average guess (/ x guess)))
(define (average a b)
	(/ (+ a b) 2))
(define (good-enough? oldGuess newGuess)
	(< (abs (- newGuess oldGuess)) (* newGuess .0000000000001)))
(define (sqrt x)
	(sqrt-iter 0 1 x))

(define (new-if predicate then-clause else-clause)
	(cond (predicate then-clause)
	      (else else-clause)))
(define (sqrt-iter2 guess x)
	(new-if (good-enough? guess x)
		guess
		(sqrt-iter (improve guess x) x)))

; When the procedure is run with new-if, it enters into an infinite loop
; The reason behind this is because procedures will use normal-order evaluation.
; As such, the recursive call to sqrt-iter2 is evaluated and entered into
; before (new-if ...) can be evaluated.  Sqrt-iter2 then calls itself again
; and goes on until max recursion depth is reached.

; Exercise 1.7
; Redesign good enough to work with small numbers
; I tested the above sqrt function on an arbitrarily small number .00000589
; and a large value, 20 9s.  Windows calculator and my sqrt procedure have
; very different answers to both.  After rewriting the procedure in the above manner,
; the values became much closer together.
