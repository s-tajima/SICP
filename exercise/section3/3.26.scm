;= Question =============================================================================
;
; 問題 3.26
;
; 上で実装した表を探すのに, レコードのリストを走査する必要がある. 
; これは基本的に2.3.3節の順序づけられていないリスト表現である. 
; 巨大な表では, 別のやり方で表を構成する方がもっと効率的かも知れない. 
; (キー, 値)のレコードが二進木を使って組織化する表の実装を述べよ. 
; キーは何らかの方法で(数値的にかアルファベット順かで)順序づけられるものと仮定する. 
; (2章の問題2.66と比べよ.)
;
;= Prepared =============================================================================


;= Answer ===============================================================================

(define (make-table)
  (let ((local-table '*table*))
       (define (key-tree tree)
         (car tree))
       (define (value-tree tree)
         (cadr tree))
       (define (left-branch tree)
         (caddr tree))
       (define (right-branch tree)
         (cadddr tree))
       (define (make-tree key value left right)
         (list key value left right))
       (define (set-value-tree! tree value)
         (set-car! (cdr tree) value))
       (define (set-left-branch-tree! tree left)
         (set-car! (cddr tree) left))
       (define (set-right-branch-tree! tree right)
         (set-car! (cdddr tree) right))

       (define (lookup key)
         (define (iter key tree)
           (cond ((null? key) #f)
                 ((= key (key-tree tree)) (value-tree tree))
                 ((< key (key-tree tree))
                  (iter key (left-branch tree)))
                 ((> key (key-tree tree))
                  (iter key (right-branch tree)))))
         (iter key local-table))

       (define (insert! key value)
         (define (make-branch key value)
           (make-tree key value '() '()))
         (define (iter key value tree)
           (cond ((eq? tree '*table*)
                  (set! local-table (make-branch key value)))
                 ((= key (key-tree tree))
                  (set-value-tree! tree value))
                 ((< key (key-tree tree))
                  (if (null? (left-branch tree))
                      (set-left-branch-tree! tree (make-branch key value))
                      (iter key value (left-branch tree))))
                 ((> key (key-tree tree))
                  (if (null? (right-branch tree))
                      (set-right-branch-tree! tree (make-branch key value))
                      (iter key value (right-branch tree))))))
         (iter key value local-table)
         'ok)

       (define (print-table)
         (display local-table)
         (newline))

       (define (dispatch m)
         (cond ((eq? m 'print-table) print-table)
               ((eq? m 'lookup-proc) lookup)
               ((eq? m 'insert-proc!) insert!)
               (else (error "Unknown operation TABLE" m))))
       dispatch))

;= Test =================================================================================

(define tb (make-table))
(define lookup (tb 'lookup-proc))
(define insert! (tb 'insert-proc!))
(define print-table (tb 'print-table))
(insert! '6 'b)
(insert! '4 'd)
(insert! '3 'e)
(insert! '1 'a)
(insert! '5 'c)
(lookup '6)
(lookup '4)
(lookup '3)
(lookup '1)
(print-table)
