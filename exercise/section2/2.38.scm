;= Question =============================================================================
;
; 問題 2.38
; 
;
; accumulate手続きは, 
; 並びの先頭の要素を右方のすべての要素を組み合せた結果に組み合せるので, 
; fold-rightとしても知られている. 
; 逆向きに仕事をしながら要素を組み合せる他はfold-rightと類似な fold-leftもある.
;
; (define (fold-left op initial sequence)
;   (define (iter result rest)
;     (if (null? rest)
;         result
;         (iter (op result (car rest))
;               (cdr rest))))
;   (iter initial sequence))
;
; 次の値は何か.
; 
; (fold-right / 1 (list 1 2 3))
; 
; (fold-left / 1 (list 1 2 3))
; 
; (fold-right list nil (list 1 2 3))
; 
; (fold-left list nil (list 1 2 3))
; 
; fold-rightとfold-leftが, 
; どのような並びに対しても同じ値を生じるためにopが満たすべき性質は何か. 
;
;= Prepared =============================================================================

(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (fold-right op initial (cdr sequence)))))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

;= Answer ===============================================================================

(print (fold-right * 1 (list 1 2 3)))
(print (fold-left  * 1 (list 1 2 3)))

(print (fold-right / 1 (list 1 2 3)))
(print (fold-left  / 1 (list 1 2 3)))

(print (fold-right - 1 (list 1 2 3)))
(print (fold-left  - 1 (list 1 2 3)))

(print (fold-right list () (list 1 2 3)))
(print (fold-left  list () (list 1 2 3)))
 
