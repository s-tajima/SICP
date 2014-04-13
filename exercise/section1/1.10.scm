(define (A x y)
	(cond ((= y 0) 0)
	      ((= x 0) (* 2 y))
		  ((= y 1) 2)
		  (else (A (- x 1)
		  		   (A x (- y 1))))))

(print (A 0 1))
(print (A 0 2))
(print (A 0 3))
(print (A 0 4))
(print (A 0 5))
(print )

(print (A 1 1))
(print (A 1 2))
(print (A 1 3))
(print (A 1 4))
(print (A 1 5))

(print )

(print (A 2 1))
(print (A 2 2))
(print (A 2 3))
(print (A 2 4))

