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

; Exercise 1.8
; Using above methods, approximate cube root with given formula
; ((x/y^2) + 2y)/3
(define (improve-y guess x)
	(/ (+ (/ x (square guess)) (* 2 guess)) 3))
	
(define (cube-root-iter prev guess x)
	(if (good-enough? prev guess x)
		guess
	(cube-root-iter guess (improve-y guess x) x)))
(define (cubeRoot x)
	(cube-root-iter 0 1 x))
	
; Using most of the same functions from above, The cubeRoot procedure works well with small and very large
; numbers as well.

; Exercise 1.9
; The first function is recursive.  It calls inc on the return value of (+ (dec a) b)
; which would in turn call (inc) again if a did not equal 0.  This would result in n=a number
; of calls to inc, giving us (inc (inc (inc ...)))
; The second function is iterative.  Every time the function is called, the values are changed
; until a = 0 and we have the answer immediately.

; Exercise 1.10
;(define (A x y)
;	(cond 
;		((= y 0) 0)
;		((= x 0) (* 2 y))
;		((= y 1) 2)
;		(else (A (- x 1)
;			(A x (- y 1))))))
	
; (A 1 10) = 1024
; (A 2 4) = 65536
; (A 3 3) = 65536
(define (f n) (A 0 n))
(define (g n) (A 1 n))
(define (h n) (A 2 n))

; f(n) = 2n
; g(n) = 2^n
; h(n) = 2^2^2^2^...n

; Exercise 1.11
; Function f(n) =n if n<3 else f(n-1) + 2f(n-2) + 3f(n-3) if n>= 3
; Write a procedure that computes f by means of a recursive and iterative process.

; Recursive:
(define (f n)
	(cond
		((< n 3) n)
		(else (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3)))))))
		
; Iterative:
;;;;;;;;;;;;;;;;
;;;KeepWorking;;
;;;;;;;;;;;;;;;;

; Exercise 1.12
; Pascal's triangle
(define (pascal row col)
	(cond
		((or (< row 0) (< col 0) (> col row)) -1) ;Error output
		((or (= row 0) (= col 0) (= col row)) 1)
		(else (+ (pascal (- row 1) col) (pascal (- row 1) (- col 1))))))
		
		
; Exercise 1.13
; Claim: Fib(n) is the closest integer to (phi^n)/(sqrt 5), phi = (1 + (sqrt 5))/2
; Prove (phi^n - psi^n)/(sqrt 5) = Fib(n)
; Base Case: Fib(0) = 0.  (phi^0 - psy^0)/(sqrt 5) = 0 OK
; Choose k in N.  Assume Fib(k) = (phi^k - psi^k)/(sqrt 5), Fib(k-1) = (phi^(k-1) - psi(k-1))/(sqrt 5)
; Show Fib(k+1) = (phi^(k+1) - psi^(k+1))/(sqrt 5)
; Fib(k+1) = Fib(k) + Fib(k-1)
; = (phi^k - psi^k)/(sqrt 5)  + (phi^(k-1) - psi(k-1))/(sqrt 5)
; = (phi^k + phi^(k-1) -psi^k - psi^(k-1))/(sqrt 5)


; Exercise 1.14
; Will draw later

; Exercise 1.15
; a How many times is procedure p applied when (sine 12.15) is evaluated?
; Procedure p gets called 5 times when 12.15 is evaluated.
; b As a function of a, what is the order of growth when evaluating (sine a)
; Every time sine recurs, it divides its argument by 3, so it would be logarithmic.

; Exercise 1.16
(define (even? n)
		(= (mod n 2) 0))
		
(define (new-fast-expt x y)
	(define (expt-iter a b n)
		(cond
			((= n 0) a)
			((even? n) (expt-iter a (* b b) (/ n 2)))
			(else (expt-iter (* b a) b (-n 1)))))
	(expt-iter 1 x y))
	
; Exercise 1.17
(define (double n) (+ n n))
(define (halve n) (/ n 2))
(define (* a b)
	(cond
		((= b 0) 0)
		((even? b) (double (* a (halve b))))
		(else (+ a (* a (- b 1))))))

; Exercise 1.18		
(define (log-multiply-iter total a b)
	(cond
		((= b 0) total)
		((even? b) (log-multiply-iter total (double a) (halve b)))
		(else (log-multiply-iter (+ a total) a (- b 1)))))
		
		
	
