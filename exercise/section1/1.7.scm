(define (square x) (* x x))

(define (sqrt-iter guess pre-guess x)
	(if (good-enough? guess pre-guess)
		guess 
  		(sqrt-iter (improve guess x)
					guess
					x)))

(define (improve guess x)
	(average guess (/ x guess)))

(define (average x y)
	(/ (+ x y) 2))

(define (good-enough? guess pre-guess)
	(< (abs (- guess pre-guess)) 0.001))
  
(define (sqrt x)
	(sqrt-iter 1.1 1.0 x))

(print (sqrt 9000000000000))
