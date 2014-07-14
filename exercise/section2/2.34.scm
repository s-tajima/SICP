;= Question =============================================================================
;  
; 問題2.34
;
; xの多項式の, あるxの値での評価は, アキュムレーションとして形式化出来る. 多項式
; 
; anxn+an-1x n-1+ ... + a1x+a0
; 
; は計算を
; 
; (... (an x+an-1)x+ ... +a1 ) x+a0
; 
; と構造化する Hornerの方法(Horner's rule)としてよく知られたアルゴリズムで評価する. 
; つまりanから始め, xを掛けan-1を足し, xを掛け, これをa0になるまで繰り返す.
; 16 下の雛型を補って, Hornerの方法で多項式を評価する手続きを作れ. 
; 多項式の係数はa0からanの順に, 並びに配置されていると仮定せよ.
; 
; (define (horner-eval x coefficient-sequence)
;   (accumulate (lambda (this-coeff higher-terms) ⟨??⟩)
;               0
;               coefficient-sequence))
; 
; 例えばx=2で, 1+3x+5x3+x5の計算は
;
; (horner-eval 2 (list 1 3 0 5 0 1))
;
; と評価する. 
; 
;= Prepared =============================================================================

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

;= Answer ===============================================================================

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))

(print (horner-eval 2 (list 1 3 0 5 0 1)))
