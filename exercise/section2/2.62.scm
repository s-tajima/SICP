;= Question =============================================================================
; 
; 問題 2.62
;
; 順序づけられたリストによる集合表現の union-setを, Θ(n)で実装せよ. 
;
;= Prepared =============================================================================

;= Answer ===============================================================================

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((= (car set1) (car set2)) (union-set (cdr set1) set2))
        ((< (car set1) (car set2)) (cons (car set1) (union-set (cdr set1) set2)))
        (else                      (cons (car set2) (union-set (cdr set2) set1)))))

(print (union-set (list 20 150) (list 10 100 1000)))
(print (union-set (list 10 150) (list 10 100 1000)))
(print (union-set (list 10 150) (list)))

