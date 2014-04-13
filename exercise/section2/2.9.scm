;= Question ================================================
;問題 2.9
;
;Alyssaと似たような推論をして, 二つの区間の差の計算法を書け. それに対応する sub-intervalという減算手続きを定義せよ.
;
;= Prepared =================================================

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y))) 
        (p3 (* (upper-bound x) (lower-bound y))) 
        (p4 (* (upper-bound x) (upper-bound y)))) 
       (make-interval (min p1 p2 p3 p4) 
                      (max p1 p2 p3 p4)))) 

(define (div-interval x y)
  (mul-interval x (make-interval (/ 1.0 (upper-bound y))
                                 (/ 1.0 (lower-bound y)))))

(define (make-interval a b) (cons a b))

(define (lower-bound a)
  (min (car a) (cdr a)))
  
(define (upper-bound a)
  (max (car a) (cdr a)))

;= Answer ==================================================

(define (width x)
  (/ (- (upper-bound x) (lower-bound x)) 2))

(print (width (make-interval 1 5)))
(print (width (add-interval (make-interval 1  5) (make-interval 0  4))))
(print (width (add-interval (make-interval 2  6) (make-interval 0  4))))
(print (width (add-interval (make-interval 7 11) (make-interval 8 12))))

(print (width (sub-interval (make-interval 1  5) (make-interval 0  4))))
(print (width (sub-interval (make-interval 2  6) (make-interval 0  4))))
(print (width (sub-interval (make-interval 7 11) (make-interval 8 12))))

(print (width (mul-interval (make-interval 1  5) (make-interval 0  4))))
(print (width (mul-interval (make-interval 2  6) (make-interval 0  4))))
(print (width (mul-interval (make-interval 7 11) (make-interval 8 12))))

(print (width (div-interval (make-interval 1  5) (make-interval 0  4))))
(print (width (div-interval (make-interval 2  6) (make-interval 0  4))))
(print (width (div-interval (make-interval 7 11) (make-interval 8 12))))
