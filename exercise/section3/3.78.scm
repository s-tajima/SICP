;= Question =============================================================================
;
; 問題 3.78
;
; 同次二階線形微分方程式
; 
; を調べる信号処理システムを設計する問題を考えよう. 
; yをモデル化した出力ストリームはループを含む回路で生成される.
; それはd2y/dt2の値がyとdy/dtの値に依存し, この両者はd2y/dt2を積分して決るからである. 
; コード化しようとするダイアグラムを図3.35に示す. 
; 
; 引数として定数a, bとdt, およびyとdy/dtの初期値y0とdy0をとり, 
; yの逐次の値のストリームを生成する手続きsolve-2ndを書け.
;
;= Prepared =============================================================================

(load "./stream.scm")

(define (integral delayed-integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (let ((integrand (force delayed-integrand)))
                      (add-streams (scale-stream integrand dt)
                                   int))))
  int)

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))


;= Answer ===============================================================================

(define (solve-2nd a b dt y0 dy0)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (add-streams (scale-stream dy a) (scale-stream y b)))
  y)

;= Test =================================================================================

(stream-head (solve-2nd 1 0 0.001 1 1) 10)
