;= Question =============================================================================
;
; 問題 4.5
; 
; Schemeにはcond節にもう一つの構文(⟨test⟩ =>⟨recipient⟩)がある. 
; ⟨test⟩が真の値に評価されると, ⟨recipient⟩が評価される. 
; 
; その値は一引数の手続きでなければならない
; この手続きは次に⟨test⟩の値に対して呼び出され, その結果がcond 式の値として返る. 
; 例えば
; 
; (cond ((assoc 'b '((a 1) (b 2))) => cadr)
;       (else false))
; 
; は2を返す. この拡張構文が使えるよう, condの処理を修正せよ. 
; 
;= Prepared =============================================================================

(load "./my-lisp.scm")

(define (expand-clauses clauses)
  (print "my expand called")
  (if (null? clauses)
      #f
      (let ((first (car clauses))
            (rest (cdr clauses)))
           (if (cond-else-clause? first)
               (if (null? rest)
                   (sequence->exp (cond-actions first))
                   (error "ELSE clause isn't last -- COND->IF"
                          clauses))
               (make-if (cond-predicate first)
                        (let ((action (cond-actions first))
                              (predicate (cond-predicate first)))
                             (if (eq? (car action) '=>)
                                 (list (cadr action) predicate)
                                 (sequence->exp action)))
                        (expand-clauses rest))))))


;= Answer ===============================================================================

(print (eval (list 'cond (list (list 1 2) '=> cadr) (list 'else (list 'false))) the-global-environment))


