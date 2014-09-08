;= Question =============================================================================
; 
; 次の各式を評価した結果, 解釈系は何を印字するか.
; 
; (list 'a 'b 'c)
; 
; (list (list 'george))
; 
; (cdr '((x1 x2) (y1 y2)))
; 
; (cadr '((x1 x2) (y1 y2)))
; 
; (pair? (car '(a short list)))
; 
; (memq 'red '((red shoes) (blue socks)))
; 
; (memq 'red '(red shoes blue socks))
; 
;= Prepared =============================================================================

;= Answer ===============================================================================

(print (list 'a 'b 'c))

(print (list (list 'george)))

(print (cdr '((x1 x2) (y1 y2))))

(print (cadr '((x1 x2) (y1 y2))))

(print (pair? (car '(a short list))))

(print (memq 'red '((red shoes) (blue socks))))

(print (memq 'red '(red shoes blue socks)))


