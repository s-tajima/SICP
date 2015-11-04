;= Question =============================================================================
;  
; 問題 3.55
; 
; 引数としてストリームSをとり, その要素がS0, S0 + S1, S0 + S1 + S2, ... 
; であるようなストリームを返す手続き partial-sumsを定義せよ. 
; 例えば(partial-sums integers) はストリーム1, 3, 6, 10, 15, ... を返すものとする.
;
;= Prepared =============================================================================

(load "./stream.scm")

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

;= Answer ===============================================================================

(define (partial-sums s)
  (cons-stream (stream-car s)
               (add-streams (partial-sums s) (stream-cdr s))))

;= Test =================================================================================

(print (stream-ref (partial-sums integers) 0))
(print (stream-ref (partial-sums integers) 1))
(print (stream-ref (partial-sums integers) 2))
(print (stream-ref (partial-sums integers) 3))
(print (stream-ref (partial-sums integers) 4))
