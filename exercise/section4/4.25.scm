;= Question =============================================================================
;
; 問題 4.25
; 
; (通常の作用的順序のSchemeで)unlessを上に示したように定義したとし, 
; factorialをunlessを使って
; 
; (define (factorial n)
;   (unless (= n 1)
;           (* n (factorial (- n 1)))
;           1))
; 
; と定義したとする. (factorial 5)を評価しようとすると何が起きるか. 
; われわれの定義は正規順序の言語では動くだろうか.
; 
;= Prepared =============================================================================


(define (unless condition usual-value exceptional-value)
  (if condition exceptional-value usual-value))

(define (factorial n)
  (print n)
  (unless (= n 1)
          (* n (factorial (- n 1)))
          1))
  
(factorial 5)

;= Answer ===============================================================================
