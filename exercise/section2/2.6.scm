(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define (inc n)
  (+ n 1))

(define (to-s z)
  ((z inc) 0))

;=================================

(add-1 zero)
;(lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) x)) f) x))))
;(lambda (f) (lambda (x) (f ((lambda (x) x) x))))

(define one (lambda (f) (lambda (x) (f x))))

(add-1 one)
(lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) (f x))) f) x))))
(lambda (f) (lambda (x) (f ((lambda (x) (f x)) x))))
(lambda (f) (lambda (x) (f (f x))))

(define two (lambda (f) (lambda (x) (f (f x)))))

(define (+ m n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(print (to-s one))
(print (to-s two))
(print (to-s two))

(define (add m n)
  (lambda (f) (lambda (x) ((m f) ((n f) x)))))

(print (to-s (add two two)))
