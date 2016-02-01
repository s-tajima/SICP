;= Question =============================================================================
;
; 問題 3.79
;
; 問題3.78のsolve-2nd手続きを一般化し, 
; 一般的な二階微分方程式d2y/dt2=f(dy/dt, y)を解くのに使えるようにせよ.
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

(define (solve-2nd dt y0 dy0 f)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (stream-map f dy y))
  y)

;= Test =================================================================================

(stream-head (solve-2nd 0.001 1 1 (lambda (x y) (* x y))) 10)
