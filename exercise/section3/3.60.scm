;= Question =============================================================================
;  
; 問題 3.60
; 
; 問題3.59のように係数のストリームとして表現したべき級数で,
; 級数の加算がadd-streamsで実装してある. 級数の乗算の次の手続きの定義を完成せよ.
; 
; (define (mul-series s1 s2)
;   (cons-stream ⟨??⟩ (add-streams ⟨??⟩ ⟨??⟩)))
; 
; この手続きは問題3.59の級数を使い, sin2x + cos2 x = 1を確認することでテスト出来る. 
; 
; 
; 
;= Prepared =============================================================================

(load "./stream.scm")


(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

;= Answer ===============================================================================

(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1)
                  (stream-car s2))
               (add-streams 
                 (stream-map (lambda (x) (*
                                           (stream-car s1)
                                           x))
                             (stream-cdr s2))
                 (mul-series (stream-cdr s1) s2))))

;= Test =================================================================================

(define a (mul-series integers integers))

(stream-head a 10)
