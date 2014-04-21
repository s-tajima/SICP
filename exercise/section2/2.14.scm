;= Question =============================================================================
;問題 2.14
;
;Lemの正しいことを示せ. いろいろな算術演算の式でシステムの振舞いを調べよ. 
;区間AとBを作り, A/AとA/Bを計算するのに使ってみよ. 
;幅が中央値に比べて小さいパーセントの区間を使うと, 大体のことは推察出来る. 
;中央値とパーセント相対許容誤差の形式の計算結果を調べよ(問題2.12参照). 
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

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1))) 
       (div-interval one
                     (add-interval (div-interval one r1)
                                   (div-interval one r2)))))

;= Answer ================================================================================

(print (par1 (make-interval 100000 100001) (make-interval 100000 100001)))
(print (par2 (make-interval 100000 100001) (make-interval 100000 100001)))
