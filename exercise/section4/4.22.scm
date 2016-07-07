;= Question =============================================================================
;
; 問題 4.22
;
; 本節の評価器を拡張し, 特殊形式letが使えるようにせよ. (問題4.6参照)
;
;= prepared =============================================================================

(load "./analyze.scm")
(load "./eval.scm")

;= answer ===============================================================================

(define (eval exp env)
  (print 'herecomes)
  ((analyze exp) env))


(define (analyze exp)
  (cond ((self-evaluating? exp)
         (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((let? exp) (analyze (let->combination exp)))
        ((application? exp) (analyze-application exp))
        (else
          (error "Unknown expression type -- ANALYZE" exp))))

;; EVAL
(evaluator)
;(print (analyze 1))
