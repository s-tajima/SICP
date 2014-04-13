(define (square x) (* x x))

(define (f g)
  (g 2))

(print (f square))
(print (f (lambda (z) (* z (+ z 1)))))

