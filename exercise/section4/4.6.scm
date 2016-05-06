;= Question =============================================================================
;
; 問題 4.6
; 
; 
;= Prepared =============================================================================

(load "./eval.scm")

;= Answer ===============================================================================

(define (eval exp env)
  (print "4.6.scm eval called")
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((and? exp) (eval-and exp env))
        ((or? exp) (eval-or exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
          (make-procedure (lambda-parameters exp) (lambda-body exp) env))
        ((begin? exp)
          (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((let? exp) (eval (let->combination exp) env))
        ((let*? exp) (eval (let*->nested-lets exp) env))
        ((while? exp) (eval (while->derived exp) env))
        ((until? exp) (eval (until->derived exp) env))
        ((do? exp) (eval (do->derived exp) env))
        ((application? exp)
          (apply-new (eval (operator exp) env)
                 (list-of-values (operands exp) env)))
        (else
          (error "Unknown expression type -- EVAL" exp))))


(define (let? exp) (tagged-list? exp 'let))

(define (let-clauses exp) (cdr exp))

(define (let-bindings clauses) (car clauses))

(define (let-body clauses) (cdr clauses))

(define (let->combination exp)
  (expand-let-clauses (let-clauses exp)))

(define (expand-let-clauses clauses)
  (if (null? (let-bindings clauses))
      '()
      (cons (make-lambda (map car (let-bindings clauses)) (let-body clauses))
            (map cadr (let-bindings clauses)))))


(evaluator)
