;= Question =============================================================================
;
; 問題 4.20
; 
; 内部定義は逐次のように見えて実は同時なので, それを全部やめ, 
; その代り特殊形式letrecを使うのがよいという人がいる. 
; 
; letrecは letのように見えるから,それが束縛する変数は同時に束縛され, 
; 互いに同じ有効範囲を持つことは驚く程ではない. 上の例題の手続きfは
; 
; (define (f x)
;   (letrec ((even?
;              (lambda (n)
;                      (if (= n 0)
;                          true
;                          (odd? (- n 1)))))
;            (odd?
;              (lambda (n)
;                      (if (= n 0)
;                          false
;                          (even? (- n 1))))))
;           ⟨fの本体の残り⟩))
; 
; のように内部定義なしで, しかも同じ意味を持つように書くことが出来る.
; 
; (letrec ((⟨var1⟩ ⟨exp1⟩) ... (⟨varn⟩ ⟨expn⟩))
;         ⟨body⟩)
; 
; の形のletrec式はletの変形で, 
; 変数⟨vark⟩の初期値となる式⟨expk⟩はletrecのすべての束縛を含む環境で評価される.
; これは上の例のeven?とodd?の相互再帰のような束縛の再帰や
; 
; (letrec ((fact
;            (lambda (n)
;                    (if (= n 1)
;                        1
;                        (* n (fact (- n 1)))))))
;         (fact 10))
; 
; の10の階乗の評価を許す.
; 
; a. letrecを上の本文や問題4.18で示したようにletrec式を let式に変換し, 
; 導出された式として実装せよ. 
; つまりletrecの変数はletで作り出され, その後その値はset!で代入される.
; 
; b. Louis Reasonerは内部定義の騒ぎで混乱した. 
; 彼の考えは, 手続きの内側でdefineが使いたくなければ, letを使うことが出来る. 
; 彼の考えで抜けていることを, fがこの問題で定義されているものとし, 
; 式(f 5)の評価の最中で⟨f⟩の本体の残りが
; 評価される環境を示す環境ダイアグラムを描いて示せ. 
; 同じ評価でfの定義のletrec がletになった場合の環境ダイアグラムを描け.
;
;= prepared =============================================================================

(load "./eval.scm")

;= answer ===============================================================================

;; EVAL
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
        ((letrec? exp) (eval (letrec->let exp) env))
        ((while? exp) (eval (while->derived exp) env))
        ((until? exp) (eval (until->derived exp) env))
        ((do? exp) (eval (do->derived exp) env))
        ((application? exp)
          (apply-new (eval (operator exp) env)
                 (list-of-values (operands exp) env)))
        (else
          (error "Unknown expression type -- EVAL" exp))))

(define (letrec? exp) (tagged-list? exp 'letrec))

(define (letrec->let exp)
  (let ((vars (map car (cadr exp)))
        (exps (map cdr (cadr exp)))
        (body (cddr exp)))
       (cons 'let
             (cons (map (lambda (x) (list x ''*unassigned*)) vars)
                   (append (map (lambda (x y) (cons 'set! (cons x y))) vars exps)
                           body)))))



(evaluator)
