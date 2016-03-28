;= Question =============================================================================
;
; 問題 4.3
; 
; データ主導流で振分け出来るようevalを書き直せ. 
; これを問題2.73のデータ主導微分プログラムと比較せよ. 
; (この節で実装した構文では, それが適切なので, 合成式のcarを式の型として使え.) 
; 
;= Prepared =============================================================================

(load "./my-lisp.scm")

;;;; eval の定義
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        (else
          (if (get 'eval (operator exp))
              ((get 'eval (operator exp)) exp env)
              (if (application? exp)
                  (apply (eval (operator exp) env)
                         (list-of-values (operands exp) env)))))))

;;;; パッケージのインストール
(define (install-eval-package)
  (define (text-of-quotation exp env) (cadr exp))
  (define (eval-assignment exp env)
    (set-variable-value! (assignment-variable exp)
                         (eval (assignment-value exp) env)
                         env)
    'ok)
  (define (assignment-variable exp) (cadr exp))
  (define (assignment-value exp) (caddr exp))
  (define (eval-difinition exp env)
    (define-variable! (definition-variable exp)
                      (eval (definition-value exp) env)
                      env)
    'ok)
  (define (definition-variable exp)
    (if (symbol? (cadr exp))
        (cadr exp)
        (caadr exp)))
  (define (definition-value exp)
    (if (symbol? (cadr exp))
        (caddr exp)
        (make-lambda (cdadr exp)      ; 仮パラメタ
                     (cddr exp))))    ; 本体
  (define (lambda-parameters exp) (cadr exp))
  (define (lambda-body exp) (cddr exp))
  (define (make-lambda parameters body)
    (cons 'lambda (cons parameters body)))
  (define (eval-if exp env)
    (if (true? (eval (if-predicate exp) env))
        (eval (if-consequent exp) env)
        (eval (if-alternative exp) env)))
  (define (if-predicate exp) (cadr exp))
  (define (if-consequent exp) (caddr exp))
  (define (if-alternative exp)
    (if (not (null? (cdddr exp)))
        (cadddr exp)
        #f))
  (define (make-if predicate consequent alternative)
    (list 'if predicate consequent alternative))
  (define (begin-actions exp) (cdr exp))
  (define (sequence->exp seq)
    (cond ((null? seq) seq)
          ((last-exp? seq) (first-exp seq))
          (else (make-begin seq))))
  (define (make-begin seq) (cons 'begin seq))
  (define (cond-clauses exp) (cdr exp))
  (define (cond-else-clause? clause)
    (eq? (cond-predicate clause) 'else))
  (define (cond-predicate clause) (car clause))
  (define (cond-actions clause) (cdr clause))
  (define (cond->if exp)
    (expand-clauses (cond-clauses exp)))
  (define (expand-clauses clauses)
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
                          (sequence->exp (cond-actions first))
                          (expand-clauses rest))))))
  (put 'eval 'quote text-of-quotation)
  (put 'eval 'set! eval-assignment)
  (put 'eval 'define eval-difinition)
  (put 'eval 'lambda (lambda (exp env) (make-procedure (lambda-parameters exp)
                                                (lambda-body exp)
                                                env)))
  (put 'eval 'if eval-if)
  (put 'eval 'begin (lambda(exp env) (eval-sequence (begin-actions exp) env)))
  (put 'eval 'cond (lambda(exp env) (eval (cond->if exp) env)))
  'done)

(install-eval-package)



;= Answer ===============================================================================

