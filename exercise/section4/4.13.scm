;= Question =============================================================================
;
; 問題 4.13
;
; Schemeでは, defineにより, 変数の新しい束縛を作り出すことが出来る. 
; しかし束縛を除去する方法は用意していない. 
; unbind!式を評価した環境から与えられた記号の束縛を除去する特殊形式unbind! を,
; 評価器に実装せよ. 
; この問題は完全な仕様にはなっていない. 
; 例えば環境の最初のフレームからだけ結合を除去するのか. 
; 仕様を完成し, 自分の選択を正当化せよ.
;
;= Prepared =============================================================================

(load "./eval.scm")

;= Answer ===============================================================================

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((unbind? exp) (eval-unbind exp env))
        ((and? exp) (eval-and exp env))
        ((or? exp) (eval-or exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
          (make-procedure (lambda-parameters exp) (lambda-body exp) env))
        ((begin? exp)
          (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((let? exp) (eval (let->derived exp) env))
        ((let*? exp) (eval (let*->nested-lets exp) env))
        ((while? exp) (eval (while->derived exp) env))
        ((until? exp) (eval (until->derived exp) env))
        ((do? exp) (eval (do->derived exp) env))
        ((application? exp)
          (apply-new (eval (operator exp) env)
                 (list-of-values (operands exp) env)))
        (else
          (error "Unknown expression type -- EVAL" exp))))


(define (unbind? exp) (tagged-list? exp 'unbind!))

(define (eval-unbind exp env) 
  (print env))

(define (eval-unbind exp env)
  (unbind-variable! (unbind-varialbe exp) env)
  'ok)

(define (unbind-varialbe exp) (cadr exp))

(define (unbind-variable! var env)
  (let ((frame (first-frame env)))
       (define (scan vars vals)
         (cond ((null? vars)
                (error "Unbound variabl --UNBIND-VARIABLE:" var))
               ((eq? var (car vars))
                (set-car! vars (cadr vars))
                (set-cdr! vars (cddr vars))
                (set-car! vals (cadr vals))
                (set-cdr! vals (cddr vals)))
               (else (scan (cdr vars) (cdr vals)))))
       (scan (frame-variables frame)
             (frame-values frame))))
      


(evaluator)
