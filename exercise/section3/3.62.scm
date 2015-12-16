;= Question =============================================================================
;  
; 問題 3.62
; 
; 問題3.60と3.61を使い, 二つのべき級数を割る手続きdiv-seriesを定義せよ. 
; 分母の級数は零でない定数項で始るとし, 任意の二つの級数について働くものとする. 
; (分母が零の定数項を持つなら, div-seriesはエラーを出すべきである.) 
; div-seriesを問題3.59の結果と共に使い, 正接のべき級数を生成する方法を述べよ.
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

(define ones (cons-stream 1 ones))

(define (scale-stream stream factor)
    (stream-map (lambda (x) (* x factor)) stream))

(define (invert-unit-series s1)
  (cons-stream 1 (mul-series (scale-stream (stream-cdr s1) -1)
                             (invert-unit-series s1)
                             )))


;= Answer ===============================================================================

(define (div-series2 s1 s2)
  (mul-series s1 (invert-unit-series s2)))

;= Test =================================================================================

(define s (div-series2 integers ones))

(stream-head s 5)
