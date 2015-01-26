;= Question =============================================================================
;
; 問題 3.2
; 
; ソフトウェアテストの応用では, 
; ある手続きが計算の過程で呼び出された回数が数えられると有用である. 
; それ自身が一入力をとる手続きfを入力としてとる手続き make-monitoredを書け. 
; make-monitoredが返す結果は三番目の手続きで, それをmfとすると, 
; mfは自分が呼び出された回数の内部カウンタに覚えている. 
; mfへの入力が特別な記号 how-many-calls?だと, mfはカウンタの値を返す. 
; 入力が特別な記号 reset-countだと, mfはカウンタを零にリセットする. 
; それ以外の入力では, mfはfをその入力で呼び出した時の結果を返し, カウンタを増やす. 
; 例えばsqrt手続きのモニタ版を作ることが出来る.
; 
; (define s (make-monitored sqrt))
; 
; (s 100)
; 10
; 
; (s 'how-many-calls?)
; 1
; 
;= Prepared =============================================================================


;= Answer ===============================================================================

(define (make-monitored f)
  (let ((num 100) (c 0))
    (lambda (num)
                (cond ((eq? num 'how-many-calls?) c)
                      ((eq? num 'reset-count) (set! c 0))
                      (else (begin (set! num (f num))
                                    (set! c   (+ c 1)) 
                                    num))))))

(define s (make-monitored sqrt))
(print (s 100))
(print (s 'how-many-calls?))
(print (s 100))
(print (s 'how-many-calls?))
(print (s 'reset-count))
(print (s 'how-many-calls?))
(print (s 100))
(print (s 'how-many-calls?))



