;= Question =============================================================================
;問題 2.10
;
;経験あるシステムプログラマ, Ben BitdiddleはAlyssaの肩越しに眺め, 
;零を跨る区間で割った時, どうなるかよく分らないと評した. 
;この状態が生じたことを調べ, 起きたらエラーとするようにAlyssaのプログラムを修正せよ.
;
;= Prepared =============================================================================

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

(define (width x)
  (/ (- (upper-bound x) (lower-bound x)) 2))

;= Answer ================================================================================

(print (width (div-interval (make-interval 1  5) (make-interval 0  4))))
(print (width (div-interval (make-interval 1  5) (make-interval -1 3))))


(define (my-div-interval x y)
  (if (< (* (lower-bound y) (upper-bound y)) 0)
    (error "error")
    (mul-interval x (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))

(print (width (my-div-interval (make-interval 1  5) (make-interval 0  4))))
(print (width (my-div-interval (make-interval 1  5) (make-interval -1 3))))
