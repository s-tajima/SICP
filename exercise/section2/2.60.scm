;= Question =============================================================================
; 
; 問題 2.60
;
; 集合を重複のないリストで表現しようとしてきた. 重複が許されるとしよう. 
; 例えば集合{1, 2, 3}はリスト(2 3 2 1 3 2 2)で表現されるかも知れない. 
; この表現を操作する手続き, element-of-set?, adjoin-set, union-set
; およびintersection-setを設計せよ. 
; 重複なし表現の対応する手続きと比べて効率はどうなるか. 
; 重複なし表現よりこの表現の方が使いたくなる応用はあるだろうか.
;
;= Prepared =============================================================================
;  
;= Answer ===============================================================================

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
      (cons x set))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)        
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else (adjoin-set (car set1) (union-set (cdr set1) set2)))))
