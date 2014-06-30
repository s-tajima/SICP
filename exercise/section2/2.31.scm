;= Question =============================================================================
;
; 問題 2.31
;
; 問題2.30の答を抽象化し, square-treeが
;
; (define (square-tree tree) (tree-map square tree))
;
; と定義出来るように, 手続き tree-mapを作れ.
;
;= Prepared =============================================================================

(define (square-tree tree) (tree-map square tree))
;= Answer ===============================================================================

(define (square i)
  (* i i))

(define (tree-map proc tree)
  (map (lambda (sub-tree)
               (if (pair? sub-tree)
                   (square-tree sub-tree)
                   (proc sub-tree)))
       tree))
   
(print (square-tree (list 1 (list 2 (list 3 4) 5) (list 6 7))))
