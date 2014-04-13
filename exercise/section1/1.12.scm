(define (pas l n)
	(cond ((= n 1) 1)
		  ((= n l) 1)	
		  (else (+ (pas (- l 1) (- n 1)) (pas (- l 1) n)))			 
	) 
)

(print (pas 2 2))
(print (pas 3 2))
