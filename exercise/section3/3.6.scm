;= Question =============================================================================
;
; 問題 3.6
; 
; 乱数発生器をリセットし, 与えられた値から始る列が作れると便利である. 
; 記号generateか記号resetのいずれかを引数として呼び出され, 
; (rand 'generate)は新しい乱数を生じ, 
; ((rand 'reset) ⟨new-value⟩)は内部状態変数を指定の⟨new-value⟩にリセットする, 
; 新しいrand手続きを設計せよ. 
; このように状態をリセットすると, 繰返し可能な列が発生出来る. 
; これがあると乱数を使うプログラムをテストしたり, 虫とりをしたりするのに楽である.
; 
;= Prepared =============================================================================


;= Answer ===============================================================================

(define rand
  (let ((x 0))
       (lambda (f)
           (if (eq? f 'generate)
               (begin (set! x (rand-update x)) x)
               (lambda (i) (set! x i))))))

(define (rand-update x) (+ x 1))

;= Test =================================================================================

(use gauche.test)
(test-start "new rand")

(test-section "case: generate")
(test* "rand?" 1 (rand 'generate))
(test* "rand?" 2 (rand 'generate))
(test* "rand?" 3 (rand 'generate))

(test-section "case: reset")
(rand 'generate)
((rand 'reset) 1)
(test* "rand?" 2 (rand 'generate))

(test-end) 
