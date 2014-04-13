(define (numer x) (car x))

(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
    (display (numer x))
      (display "/")
        (display (denom x)))

;=======================================

(define (make-rat n d) (cons n d))

(define (make-rat-n n d) 
  ( cons 
    (if (> (* n d) 0)
      (abs n)
      (* -1 (abs n)))
    (abs d)
    )) 

(print-rat (make-rat-n 1 1))
(print-rat (make-rat-n -1 -1))
(print-rat (make-rat-n -1 1))
(print-rat (make-rat-n 1 -1))
