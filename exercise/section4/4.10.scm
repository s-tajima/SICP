;= Question =============================================================================
;
; 問題 4.10
;  
; データ抽象を使うと, 評価される言語の特定の構文と独立なeval手続きを書くことが出来る.
; これを示すため, evalとapplyを変えずに, 本節の手続きを修正し,
; Schemeの新しい構文を設計し, 実装せよ.
; 
;= Prepared =============================================================================

(load "./eval.scm")

;= Answer ===============================================================================


(define (while->derived exp)
  (define (while-condition exp) (cddr exp))
  (define (while-body exp) (cadr exp))
  (make-begin
    (list
      ; 1. define our loop
      (make-define '(loop)
        (list
          (make-if (while-condition exp)
            (make-begin
              (list
                (make-begin (while-body exp))
                '(loop))) ; recursively call our loop
                "done")))
      ; 2. enter our loop
      '(loop))))



(evaluator)
