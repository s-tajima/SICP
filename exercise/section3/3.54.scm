;= Question =============================================================================
;  
; 問題 3.54
; 
; add-streamsに似て, 二つのストリームの要素毎の積を生じる手続き mul-streamsを定義せよ. 
; これとintegersストリームを使い, 
; (0から数えて)n番目の要素がn + 1の階乗になる次のストリームの定義を完成せよ:
; 
; (define factorials (cons-stream 1 (mul-streams ⟨??⟩ ⟨??⟩)))
;
;= Prepared =============================================================================

(load "./stream.scm")

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 2))

;= Answer ===============================================================================

(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define factorials (cons-stream 1 (mul-streams factorials integers)))

;= Test =================================================================================

(print (stream-ref factorials 0))
(print (stream-ref factorials 1))
(print (stream-ref factorials 2))
(print (stream-ref factorials 3))
(print (stream-ref factorials 4))
(print (stream-ref factorials 5))
(print (stream-ref factorials 6))
