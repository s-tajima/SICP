;= Question =============================================================================
;問題 2.11
;
;中央値とパーセント相対許容誤差をとり, 
;望み通りの区間を返す構成子 make-center-percentを定義せよ. 
;また区間のパーセント相対許容誤差を返す選択子percentを定義しなければならない. 
;center選択子は上に示したのと同じでよい.
;
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

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

;= Answer ================================================================================

(define (make-center-percent c p)
  (let ((e (* c (* p 0.01)))) (make-interval (- c e) (+ c e))))

(print (make-center-percent 10 10))
  
(define (persent i)
  (* (/ (/ (- (upper-bound i) (lower-bound i)) 2) (center i)) 100))

(print (make-center-percent 10 (persent (make-interval 9 11))))
