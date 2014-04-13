(define (my-cons a b)
  (* (expt 2 a) (expt 3 b)))

(print (my-cons 1 2))

(define (my-car z)
  (define (iter-2 z)
    (if (= (modulo z 2) 0) (iter (/ z 2)) z))
  (iter z))

(define (my-car z)
  (define (iter-3 z)
    (if (= (modulo z 3) 0) (iter (/ z 3)) z))
  (iter z))

(print (my-car (my-cons 1 2)))
