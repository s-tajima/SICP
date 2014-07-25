;= Question =============================================================================
;
; 問題 2.36
; 
; 手続きaccumulate-nは第三引数としてすべてが同数の要素からなる並びの並びをとる他は
; accumulateと同じである. 
; 
; これはアキュムレーションとして指定した手続きを, 並びのすべての第一要素, 
; すべての第二要素というふうに作用させ, 結果の並びを返す. 
; 
; 例えばsが四つの並びを含む並び((1 2 3) (4 5 6) (7 8 9) (10 11 12))だとすると, 
; (accumulate-n + 0 s)の値は並び(22 26 30)になる. 
; 
; 次の accumulate-nの定義の欠けた式を補え.
; 
; (define (accumulate-n op init seqs)
;   (if (null? (car seqs))
;       nil
;       (cons (accumulate op init ⟨??⟩)
;             (accumulate-n op init ⟨??⟩))))
; 
;= Prepared =============================================================================

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define s (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))

;= Answer ===============================================================================

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      ()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(print (accumulate-n + 0 s))
