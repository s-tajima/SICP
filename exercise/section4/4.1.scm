;= Question =============================================================================
;
; 問題 4.1
; 
; 超循環評価器が被演算子を左から右へ評価するか, 
; 右から左へ評価するかわれわれには分らない. 評価順は基盤のLispから引き継いでいる. 
; list-of-valuesのconsの引数が左から右へ評価されるなら, 
; list-of-valuesも被演算子を左から右へ評価する; consの引数が右から左へ評価されるなら,
; list-of-valuesも被演算子を右から左へ評価する.
; 
; 基盤のLispの評価の順と無関係に被演算子を左から右へ評価するlist-of-valuesを書け. 
; また被演算子を右から左へ評価するlist-of-valuesを書け. 
; 
;= Prepared =============================================================================

(define val 10)
(define expression '((set! val (+ val 2)) (set! val (* val 2))))

(define val2 10)
(define expression2 '((set! val2 (+ val2 2)) (set! val2 (* val2 2))))

(print expression)

(define (list-of-values-right exps)
  (if (null? exps)
      '()
      (let ((first-eval (eval (car exps) (interaction-environment))))
           (cons first-eval
                 (list-of-values-right (cdr exps))))))

;(print (list-of-values-right expression))


(define (list-of-values-left exps)
  (if (null? exps)
      '()
      (let ((first-eval (list-of-values-left (cdr exps))))
           (cons (eval (car exps) (interaction-environment))
                 first-eval))))

;(print (list-of-values-left expression2))



;= Answer ===============================================================================

(define no-operands? null?)
(define first-operand car)
(define rest-operands cdr)

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((first-eval (eval (first-operand exps) env)))
           (cons first-eval
                 (list-of-values (rest-operands exps) env)))))

; (define (list-of-values exps env)
;   (if (no-operands? exps)
;       '()
;       (let ((first-eval (list-of-values (rest-operands exps) env)))
;            (cons (eval (first-operand exps) env)
;                  first-eval))))


(define val3 10)
(define expression3 '((set! val3 (+ val3 2)) (set! val3 (* val3 2))))


(print (list-of-values expression3 (interaction-environment)))
