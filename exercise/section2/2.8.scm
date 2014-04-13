;= Question ================================================
;問題 2.8
;
;Alyssaと似たような推論をして, 二つの区間の差の計算法を書け. それに対応する sub-intervalという減算手続きを定義せよ.
;
;= Prepared =================================================

(define (lower-bound a)
  (min (car a) (cdr a)))
  
(define (upper-bound a)
  (max (car a) (cdr a)))

(define (make-interval a b) (cons a b))

;= Answer ==================================================
; 差の最小値は二つの下限の差であり, 最大値は二つの上限の差であると考えた. 

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

(print (sub-interval (make-interval 1 5) (make-interval 2 3)))
