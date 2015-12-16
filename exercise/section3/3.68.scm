;= Question =============================================================================
; 
; 問題 3.68
; 
; Louis Reasonerは対のストリームを三つの部分から作るのは必要以上に複雑だと考えた. 
; 対(S0, T0)を第一行の残りの対から分離する代り, 
; 次のようにして第一行全体を使うことを提案した:
; 
; (define (pairs s t)
;   (interleave
;     (stream-map (lambda (x) (list (stream-car s) x))
;                 t)
;     (pairs (stream-cdr s) (stream-cdr t))))
; 
; これは動くか. Louisのpairsの定義を使って(pairs integers integers)を評価すると, 
; 何が起きるか考えよ.
; 
;= Prepared =============================================================================

(load "./stream.scm")


(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

;= Answer ===============================================================================

(define (pairs s t)
  (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                t)
    (pairs (stream-cdr s) (stream-cdr t))))


(define (pairs s t)
  (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                t)
    (pairs (stream-cdr s) (stream-cdr t))))

;= Test =================================================================================

(stream-head  (pairs integers integers) 10)


