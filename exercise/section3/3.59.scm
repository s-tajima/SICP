;= Question =============================================================================
;  
; 問題 3.59
; 
; 2.5.3節で多項式演算システムが多項式を項のリストで表現するよう実装されるのを見た. 
; 同様にして
; 
; のような無限ストリームとして表現されたべき級数(power series)が扱える. 
; 級数a0 + a1x + a2x2 + a3x3 + ... を, 
; その要素が係数a0, a1, a2, a3, ... であるストリームとして表現しよう.
; 
; a. 級数a0 + a1x + a2x2 + a3x3 + ... の積分はcを任意の定数として
; 
; である. 入力ストリームとしてべき級数を表現するa0, a1, a2, ... をとり, 
; 級数の積分の定数項を除いた項の係数ストリームa0, a1, a2, ... を返す手続き
; integrate-seriesを定義せよ.
; 
; b. 関数x exは自分と同じ微分を持つ. 
; つまりexとexの積分は, e0 = 1の定数項を除いて同じ級数である. 
; 従ってexの級数を
; 
; (define exp-series
;   (cons-stream 1 (integrate-series exp-series)))
; 
; で生成出来る. 正弦の微分が余弦であり, 
; 余弦の微分が正弦の符号を変えたものであるという事実から出発し, 
; 正弦と余弦の級数を生成する方法を示せ.
; 
; (define cosine-series
;   (cons-stream 1 ⟨??⟩))
; 
; (define sine-series
;   (cons-stream 0 ⟨??⟩))
; 
; 
;= Prepared =============================================================================

(load "./stream.scm")

(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

;= Answer ===============================================================================

(define (divers-starting-from n)
  (cons-stream (/ 1 n) (divers-starting-from (+ n 1))))

(define divers (divers-starting-from 1))

(define (integrate-series s)
  (mul-streams s divers))

(define ones (cons-stream 1 ones))
(define twos (cons-stream 2 twos))

(define minus-ones (cons-stream -1 minus-ones))


(define cosine-series
  (cons-stream 1 (mul-streams minus-ones (integrate-series sine-series))))

(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))




;= Test =================================================================================

(print (stream-head (integrate-series ones) 10))

(print (stream-head (integrate-series twos) 10))

(print (stream-head sine-series 20))
(print (stream-head cosine-series 20))
