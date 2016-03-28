;= Question =============================================================================
;
; 問題 4.4
;
; 1章にあった特殊形式andとorの定義を思い出そう:
; 
; and: 
; 式を左から右へ評価する. ある式が偽に評価されたら偽を返す; 
; 残りの式は評価しない. すべての式が真に評価されたら, 最後の式の値を返す.
; 式が一つもなければ真を返す.
; 
; or:
; 式を左から右へ評価する. ある式が真の値に評価されたらその値を返す;
; 残りの式は評価しない. すべての式が偽に評価されるか, 式が一つもなければ偽を返す.
; 
; 
; 適切な構文手続きと評価手続きeval-andとeval-orを作り, 
; andとorを評価器の新しい特殊形式として組み込め. 
; またandとorを導出された式として評価する方法を示せ.
; 
; 
;= Prepared =============================================================================

;(define (eval-assignment exp env)
;  (set-variable-value! (assignment-variable exp)
;                       (eval (assignment-value exp) env)
;                       env)
;  'ok)
; 
;(define (assignment? exp)
;  (tagged-list? exp 'set!))
;
;(define (assignment-variable exp) (cadr exp))
;
;(define (assignment-value exp) (caddr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
        (eq? (car exp) tag)
              #f))

(define (self-evaluating? exp)
  (cond ((number? exp) #t)
        ((string? exp) #f)
        (else #f)))

(define (mytrue? x)
  (not (eq? x #f)))

(define (myfalse? x)
  (eq? x #f))

(load "./my-lisp.scm")

;= Answer ===============================================================================

(define (my-eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((and? exp) (eval-and (and-clauses exp) env))
        ((or? exp) (eval-or (or-clauses exp) env))
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp) 
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (my-eval (cond->if exp) env))
        ((application? exp)
         (apply (my-eval (operator exp) env)
                (list-of-values (operands exp) env)))
        ((mytrue? exp) #t)
        ((myfalse? exp) #f)
        (else
          (error "Unknown expression type -- EVAL" exp))))


;and
(define (and? exp) (tagged-list? exp 'and))

(define (and-clauses exp) (cdr exp))

(define (and-first-exp exp) (car exp))

(define (and-rest-exps exp) (cdr exp))

(define (eval-and exp env)
  (define (eval-and-iter exp result)
    (if (null? exp)
        result
        (let ((first-eval (my-eval (and-first-exp exp) env))
              (rest (and-rest-exps exp)))
             (if (mytrue? first-eval)
                 (eval-and-iter rest first-eval)
                 #f))))
  (if (null? exp)
      #t
      (eval-and-iter exp '())))


(define (or? exp) (tagged-list? exp 'or))

(define (or-clauses exp) (cdr exp))

(define (or-first-exp exp) (car exp))

(define (or-rest-exps exp) (cdr exp))

(define (eval-or exp env)
  (if (null? exp)
      #f
      (let ((first-eval (my-eval (or-first-exp exp) env))
            (rest (or-rest-exps exp)))
           (if (mytrue? first-eval)
               first-eval
               (eval-or rest env)))))


(print (my-eval (list 'and #t #t) '()))
(print (my-eval (list 'and #t #f) '()))
(print (my-eval (list 'and #t #f) '()))
(print (my-eval (list 'and #t #f) '()))
(print (my-eval (list 'and) '()))








