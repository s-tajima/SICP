;= Question =============================================================================
;  
; 問題 3.52
; 
; 次の一連の式を考える.
; 
; (define sum 0)
; 
; (define (accum x)
;   (set! sum (+ x sum))
;   sum)
; 
; (define seq (stream-map accum (stream-enumerate-interval 1 20)))
; (define y (stream-filter even? seq))
; (define z (stream-filter (lambda (x) (= (remainder x 5) 0))
;                          seq))
; 
; (stream-ref y 7)
; 
; (display-stream z)
;
; 上の各式が評価し終った時, sumの値は何か. 
; stream-refとdisplay-stream式の評価に応じて印字される応答は何か. 
; この応答は(delay ⟨exp⟩)を単に(lambda () ⟨exp⟩)で実装し, 
; memo-procの用意する最適化を使わなかった時とどう違うか. 説明せよ.
;
;= Prepared =============================================================================

(load "./stream.scm")

;= Answer ===============================================================================

(define sum 0)

(define (accum x)
  (set! sum (+ x sum))
  sum)

(define seq (stream-map accum (stream-enumerate-interval 1 20)))

(define y (stream-filter even? seq))

(define z (stream-filter (lambda (x) (= (remainder x 5) 0))
                         seq))

(display-stream z)

(print)
(print sum)

;= Test =================================================================================
