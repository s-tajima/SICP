;= Question =============================================================================
;
; 問題 5.2
; 
; レジスタ計算機言語を使い, 問題5.1の反復的な階乗計算機を記述せよ. 
; 
;= Answer ================================================================================

(controller
  init
    (assign p (const 1))
    (assign c (const 1))
  test-n
    (test (op >) (reg n) (reg c))
    (branch (label fact-done))
    (assign p (op *) (reg c) (reg p))
    (assign c (op +) (reg c) (const 1))
    (goto (label test-n))
  fact-done)

