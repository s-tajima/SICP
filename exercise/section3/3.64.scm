;= Question =============================================================================
;  
; 引数としてストリームと数値(許容誤差)をとる手続きstream-limitを書け. 
; それはストリームを調べて相続く二つの要素が絶対値で許容誤差より小さくなるのを見つけ, 
; その二つの要素の二番目を返す. 
; 
; これを使えば
; 
; (define (sqrt x tolerance)
;   (stream-limit (sqrt-stream x) tolerance))
; 
; により, 平方根を与えられた許容誤差まで計算出来る. 
; 
;= Prepared =============================================================================

(load "./stream.scm")

(define (sqrt-improve guess x)
  (average guess (/ x guess)))


(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess)
                                     (sqrt-improve guess x))
                             guesses)))
  guesses)

(define (average x y)
	(/ (+ x y) 2))

 (define (sqrt x tolerance)
   (stream-limit (sqrt-stream x) tolerance))

;= Answer ===============================================================================

(define (stream-limit s tolerance)
  (let ((a (stream-car s)) 
        (b (stream-car (stream-cdr s))))
       (if (< (abs (- a b)) tolerance)
           b 
           (stream-limit (stream-cdr s) tolerance))))


;= Test =================================================================================

(print (sqrt 3 0.000001))


