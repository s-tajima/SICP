;= Question =============================================================================
;
; 問題 5.8
; 
; 上の機械演算の扱いは定数やレジスタの内容の他ラベルにも演算することを許す. 
; 式の処理の手続きを修正し, 
; 演算はレジスタと定数にだけ使えるという条件を強要するようにせよ. 
; 
;= Answer ================================================================================

(load "./machine.scm")

;(define (make-operation-exp exp machine labels operations)
;  (let ((op (lookup-prim (operation-exp-op exp) operations))
;        (aprocs
;          (map (lambda (e)
;                       (make-primitive-exp e machine labels))
;               (operation-exp-operands exp))))
;       (lambda ()
;               (apply op (map (lambda (p) (p)) aprocs)))))

(define (make-operation-exp exp machine labels operations)
  (let ((op (lookup-prim (operation-exp-op exp) operations))
        (aprocs
          (map (lambda (e)
                       (if (label-exp? e)
                           (error "Operations can be used only with registers and constants -- ASSEMBLE" e)
                           (make-primitive-exp e machine labels)))
               (operation-exp-operands exp))))
       (lambda ()
               (apply op (map (lambda (p) (p)) aprocs)))))

(define test-machine
  (make-machine
    '()
    (list (list '= =))
    '(start
       (test (op =) (label start) (label start)))))


(print (start test-machine))

