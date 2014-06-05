;= Question =============================================================================
;
; 問題2.18の手続きreverseを修正し, 引数としてリストをとり,  
; 要素を逆順にし, 更に部分木も奥まで逆順にする手続き deep-reverseを作れ. 
; 例えば
; 
; (define x (list (list 1 2) (list 3 4)))
; 
; x
; ((1 2) (3 4))
; 
; (reverse x)
; ((3 4) (1 2))
; 
; (deep-reverse x)
; ((4 3) (2 1))
;
;= Prepared =============================================================================
(define (my-reverse l)
  (append
    (if (= (length l) 1) (list) (my-reverse (cdr l)))
    (list (car l))))

(define x (list (list 1 2) (list 3 4)))
(define y (list 1 2 3 4))
(define z (list (list 1 2 3 4) (list 1 (list 6 5))))
;= Answer ===============================================================================
(define (my-deep-reverse l)
  (if (not (pair? l)) l
    (append 
      (my-deep-reverse (cdr l))
      (list (my-deep-reverse (car l))) )))

(print (my-deep-reverse x))
(print (my-deep-reverse y))
(print (my-deep-reverse z))

