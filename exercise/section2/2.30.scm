;= Question =============================================================================
;
; 問題 2.30
;
; 問題2.21のsquare-list手続きと類似の手続きsquare-treeを定義せよ. 
; つまりsquare-treeは, 次のように振舞わなければならない.
;
; (square-tree
;   (list 1
;         (list 2 (list 3 4) 5)
;         (list 6 7)))
; (1 (4 (9 16) 25) (36 49))
;
; square-treeを直接に(つまり高階手続きを使わずに), またmapと再帰を使って定義せよ. 
; 
;= Prepared =============================================================================

;= Answer ===============================================================================

(define (square-tree1 tree)
  (cond ((null? tree) ())
        ((not (pair? tree)) (* tree tree))
        (else (cons (square-tree1 (car tree))
                    (square-tree1 (cdr tree))))))

(print (square-tree1 (list 1 (list 2 (list 3 4) 5) (list 6 7))))


(define (square-tree2 tree)
  (map (lambda (sub-tree)
               (if (pair? sub-tree)
                   (square-tree2 sub-tree)
                   (* sub-tree sub-tree)))
       tree))

(print (square-tree2 (list 1 (list 2 (list 3 4) 5) (list 6 7))))
