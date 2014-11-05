;(print (cons '+ (list 2 3)))        ; ◯
;(print (list '+ (list 2 3)))        ; ☓
;(print (append (list '+) (list 2 3)))    ; ◯

(define (exp-cdr p)
  (append (list (car p)) (cddr p)))

(define (augend p)
  (if (null? (cdddr p))
      (cadr p)
      (exp-cdr p)))
 
 (print (augend '(+ 1 2)))
