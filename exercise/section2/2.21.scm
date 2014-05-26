;= Question =============================================================================
; 問題 2.21
; 
; 手続きsquare-listは引数として数のリストをとり, これらの数の二乗のリストを返す.
; 
; (square-list (list 1 2 3 4))
; (1 4 9 16)
; 
; ここにsquare-listの二つの定義がある. 欠けている式を補い, それぞれを完成せよ:
; 
; (define (square-list items)
;    (if (null? items)
;        nil
;        (cons ⟨??⟩ ⟨??⟩)))
; 
; (define (square-list items)
;   (map ⟨??⟩ ⟨??⟩))
;
;= Prepared =============================================================================


;= Answer ===============================================================================
(define (square-list-1 items)
   (if (null? items)
       ()
       (cons (* (car items) (car items)) (square-list-1 (cdr items)))))

(define (square-list-2 items)
  (map (lambda (x) (* x x)) items))


(print (square-list-1 (list 1 3 5 9)))
(print (square-list-2 (list 1 3 5 9)))
