;= Question =============================================================================
;  
; 問題 3.61
; 
; Sを定数項が1のべき級数(問題3.59)とする. 
; べき級数1/S, つまりS... X = 1 になる級数Xが見つけたいとしよう. 
; SRをSの定数項の後の部分とし, S = 1 + SRと書く. Xは次のように解ける:
; 
; いいかえればXは定数項が1で, 
; 高次の項はSR掛けるXの符号を変えたもののべき級数である. 
; この考え方を使い, 定数項が1のべき級数Sについて, 
; 1/Sを計算する手続きinvert-unit-seriesを書け. 
; 
; 問題3.60のmul-seriesを使う必要がある.
; 
;= Prepared =============================================================================

(load "./stream.scm")


(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))


(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1)
                  (stream-car s2))
               (add-streams 
                 (stream-map (lambda (x) (*
                                           (stream-car s1)
                                           x))
                             (stream-cdr s2))
                 (mul-series (stream-cdr s1) s2))))

;= Answer ===============================================================================

(define ones (cons-stream 1 ones))

(define (scale-stream stream factor)
    (stream-map (lambda (x) (* x factor)) stream))

(define (invert-unit-series s1)
  (cons-stream 1 (mul-series (scale-stream (stream-cdr s1) -1)
                             (invert-unit-series s1)
                             )))

(define inverse-ones (invert-unit-series ones))

;= Test =================================================================================
(stream-head inverse-ones 5)
