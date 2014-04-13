(use slib)

(define (f n)
	(cond ((< n 3) n)
		(else (+ (f (- n 1)) (* (f (- n 1)) 2) (* (f (- n 3)) 3)))))
	
(trace f)
(print (f 3))

(define (f2 n)
	(f-iter 0 1 2 n))

(define (f-iter a b c n)
	(if (= n 0)
		a	
		(f-iter b c (+ c (* 2 b) (* 3 a)) (- n 1))
	)
)
(print (f2 0))
(print (f2 1))
(print (f2 2))
(print (f2 3))
(print (f2 4))
(print (f2 5))
(print (f2 6))


