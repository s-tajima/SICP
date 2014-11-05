;= Question =============================================================================
; 
; 問題 2.59
;
; 集合の順序づけられないリスト表現で union-set演算を実装せよ.
;
;= Prepared =============================================================================
;  
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)        
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))
;
;= Answer ===============================================================================


(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else (adjoin-set (car set1) (union-set (cdr set1) set2)))))

(print (union-set (list 1 2 3) (list 11 12 13)))
(print (union-set (list) (list 11 12 13)))
(print (union-set (list 1 2 3) (list)))
