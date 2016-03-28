;= Question =============================================================================
;
; 問題 4.2
; 
; Louis Reasonerはevalの中のcond節の順序を変え, 
; 手続き作用の節が代入の節の前に現れるようにしようと考えた. 
; 彼はこうすると, 解釈系の効率が高まると主張した. 
; 
; プログラムには代入や定義などより, 手続き作用の方が多いからで, 
; 彼は元々のevalより少ない節を調べて式の型が識別出来るようevalを修正した.
; 
; a. Louisの計画では何が悪かったか. 
; (ヒント: 式(define x 3)に対して, Louisの評価器は何をするか.)
; 
; b. Louisは自分の考えがうまくいかないので驚いた. 
; 評価器が, 式の他の型の大部分を調べる前に, 手続き作用を認識するなら, 
; プログラムがどんなに長くなっても構わなかった. 
; 評価される言語の構文を変更し, 手続き作用がcallで始るようにして彼を助けよ. 
; 例えば(factorial 3)の代りに, 今は(call factorial 3)と書かなければならず, 
; (+ 1 2)の代りに, (call + 1 2)と書かなければならない.
; 
;= Prepared =============================================================================

(load "./my-lisp.scm")

; (define (eval exp env)
;   (cond ((self-evaluating? exp) exp)
;         ((variable? exp) (lookup-variable-value exp env))
;         ((quoted? exp) (text-of-quotation exp))
;         ((assignment? exp) (eval-assignment exp env))
;         ((definition? exp) (eval-definition exp env))
;         ((if? exp) (eval-if exp env))
;         ((lambda? exp)
;          (make-procedure (lambda-parameters exp)
;                          (lambda-body exp)
;                          env))
;         ((begin? exp) 
;          (eval-sequence (begin-actions exp) env))
;         ((cond? exp) (eval (cond->if exp) env))
;         ((application? exp)
;          (apply (eval (operator exp) env)
;                 (list-of-values (operands exp) env)))
;         (else
;           (error "Unknown expression type -- EVAL" exp))))


;= Answer ===============================================================================

; 4.2.b
 (define (eval exp env)
   (cond ((self-evaluating? exp) exp)
         ((call? exp) (apply (eval (operator (cdr exp)) env) (list-of-values (operands (cdr exp)) env)))
         ((variable? exp) (lookup-variable-value exp env))
         ((quoted? exp) (text-of-quotation exp))
         ((assignment? exp) (eval-assignment exp env))
         ((definition? exp) (eval-definition exp env))
         ((if? exp) (eval-if exp env))
         ((lambda? exp) (make-procedure (lambda-parameters exp) (lambda-body exp) env))
         ((begin? exp) (eval-sequence (begin-actions exp) env))
         ((cond? exp) (eval (cond->if exp) env))
         (else
           (error "Unknown expression type -- EVAL" exp))))
