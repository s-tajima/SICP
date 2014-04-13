;= Question =============================================================================
;問題 2.11
;
;ついでにBenは謎めいたことをいった: 
;「区間の端点の符号を調べると, mul-intervalを九つの場合に分けることが出来, 
;そのうち一つだけが二回を超える乗算を必要とする.」
;Benの提案に従い, 手続きを書き直せ.
;
;虫とりがすむとAlyssaはこのプログラムを使ってくれそうな利用者に見せたが, 
;彼はこのプログラムは違った問題を解いていると文句をいった. 
;彼が欲しかったのは, 中央値と許容誤差で表す数を扱うプログラムであった. 
;例えば[3.35, 3.65]ではなく, 3.5 ± 0.15という区間で仕事がしたいのだ. 
;Alyssaは席に戻り, 別の構成子と選択子を用意してこの問題を解決した.
;
; (define (make-center-width c w)
;   (make-interval (- c w) (+ c w)))
; 
; (define (center i)
;   (/ (+ (lower-bound i) (upper-bound i)) 2))
; 
; (define (width i)
;   (/ (- (upper-bound i) (lower-bound i)) 2))
;
;困ったことに, Alyssaの利用者の殆んどは技術者であった. 
;実際の工学の状況では, 測定は区間の中央値に対する, 
;区間の幅の比で計る, 通常小さい不確かさを使う.
;技術者は初めの抵抗の例のように, 装置のパラメタをパーセント相対許容誤差で規定する.
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

; パターン
; (+, +, +, +)
; (+, +, -, -)
; (-, -, +, +)
; (+, +, -, +)
; (-, -, -, +)
; (-, +, -, -)
; (-, +, +, +)
; (-, -, -, -)
; (-, +, -, +)


(define (mul-interval2 x y)
  (let ((xl (lower-bound x))
        (xu (upper-bound x))
        (yl (lower-bound y))
        (yu (upper-bound y)))
  (cond ((and (<= 0 xl) (<= 0 yl))          (make-interval (* xl yl) (* xu yu)))
        ((and (<= 0 xl) (< yl 0) (<= 0 yu)) (make-interval (* xu yl) (* xu yu)))
        ((and (<= 0 xl) (< yu 0))           (make-interval (* xu yl) (* xl yu)))
        ((and (< xl 0) (<= 0 xu) (<= 0 yl)) (make-interval (* xl yu) (* xu yu)))
        ((and (< xl 0) (<= 0 xu) (< yl 0) (<= 0 yu)) (make-interval (min (* xu yl) (* xl yu)) (max (* xl yl) (* xu yu))))
        ((and (< xl 0) (<= 0 xu) (< yu 0))  (make-interval (* xu yl) (* xl yl)))
        ((and (< xu 0) (<= 0 yl))           (make-interval (* xl yu) (* xu yl)))
        ((and (< xu 0) (< yl 0) (<= 0 yu))  (make-interval (* xl yu) (* xl yl)))
        ((and (< xu 0) (< yu 0))            (make-interval (* xl yl) (* xu yu))))))

