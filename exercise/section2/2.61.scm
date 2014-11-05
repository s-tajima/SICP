;= Question =============================================================================
; 
; 問題 2.61
;
; 順序づけられた表現を使った adjoin-setの実装を示せ. 
; element-of-set?の類推で, 順序づけられない表現に比べ, 
; 約半分のステップ数を必要とする手続きを作るのに, 順序の利点をどう用いるか示せ.
;
;= Prepared =============================================================================
  
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (car set)) #t)
        ((< x (car set)) #f)
        (else (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()    
      (let ((x1 (car set1)) (x2 (car set2)))
           (cond ((= x1 x2)
                  (cons x1
                        (intersection-set (cdr set1)
                                          (cdr set2))))
                 ((< x1 x2)
                  (intersection-set (cdr set1) set2))
                 ((< x2 x1)
                  (intersection-set set1 (cdr set2)))))))

;= Answer ===============================================================================

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))

(print (adjoin-set 20 (list 10 100 1000)))
(print (adjoin-set 10 (list 10 100 1000)))
(print (adjoin-set 10 (list)))

