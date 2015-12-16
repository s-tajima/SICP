;= Question =============================================================================
;  
; 問題 3.63
; 
; Louis Reasonerは, sqrt-stream手続きは局所変数guessesを使わず, 
; 次のように直截な方法で書けなかったかと質問した.
; 
; (define (sqrt-stream x)
;   (cons-stream 1.0
;                (stream-map (lambda (guess)
;                                    (sqrt-improve guess x))
;                            (sqrt-stream x))))
; 
; Alyssa P. Hackerは手続きのこの版は, 冗長な計算をするので, かなり非効率だと答えた. 
; Alyssaの答を説明せよ. 
; delayの実装が, 
; memo-proc(3.5.1節)の用意した最適化を使わず(lambda () ⟨exp⟩)だけを使ったとしたら, 
; 二つの版の効率に違いはあるか. 
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



(define (sqrt-stream2 x)
  (cons-stream 1.0
               (stream-map (lambda (guess)
                                   (print x)
                                   (sqrt-improve guess x))
                           (sqrt-stream2 x))))


(define (average x y)
	(/ (+ x y) 2))

;= Answer ===============================================================================

(print 'sqrt-stream)
(stream-head (sqrt-stream 5) 5)

(print)
(print 'sqrt-stream2)
(stream-head (sqrt-stream2 5) 5)

;= Test =================================================================================


