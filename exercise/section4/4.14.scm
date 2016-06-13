;= Question =============================================================================
;
; 問題 4.14
; 
; Eva Lu AtorとLouis Reasonerはそれぞれ超循環評価器の実験をしていた. 
; Eva はmapの定義を入力し, それを使うテストプログラムをいくつか走らせた. 
; それはうまく動いた. 
; 反対にLouisはmapのシステム版を超循環評価器の基本手続きとして組み込んだ.
; 彼がやってみると, 非常におかしい. 
; Evaのは動いたのに, Louisのmapは失敗した理由を説明せよ.
;
;= prepared =============================================================================

(load "./eval.scm")

;= answer ===============================================================================


(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'list list)
        (list 'null? null?)
        (list 'not not)
        (list '= =)
        (list '< <)
        (list '> >)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list 'symbol? symbol?)
        (list 'display display)
        (list 'print print)
        (list 'newline newline)
        (list 'map map)
        ; ... we can add more
        ))

(define (primitive-procedure-names)
  (map car primitive-procedures))
(define (primitive-procedure-objects)
  (map
    (lambda (proc) (list 'primitive (cadr proc)))
    primitive-procedures))

(define (setup-environment)
  (let ((initial-env
         (extend-environment
           (primitive-procedure-names)
           (primitive-procedure-objects)
           the-empty-environment)))
    (define-variable! 'true #t initial-env)
    (define-variable! 'false #f initial-env)
    initial-env))

(define the-global-environment (setup-environment))

(define (eval exp env)
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
        ((let? exp) (eval (let->derived exp) env))
        ((let*? exp) (eval (let*->nested-lets exp) env))
        ((while? exp) (eval (while->derived exp) env))
        ((until? exp) (eval (until->derived exp) env))
        ((do? exp) (eval (do->derived exp) env))
        ((application? exp)
          (apply-new (eval (operator exp) env)
                 (list-of-values (operands exp) env)))
        (else
          (error "unknown expression type -- eval" exp))))




(evaluator)
