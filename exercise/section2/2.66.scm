;= Question =============================================================================
; 
; 問題 2.66
; 
; レコードの集合がキーの数値で順序づけられている二進木で
; 構造化されている場合のlookup手続きを実装せよ．
;
;= Prepared =============================================================================

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))


;= Answer ===============================================================================

(define tree1  
  (list (list 3 10)  
        (list (list 2 30) (list (list 1 10) () ()) ()) 
        (list (list 4 40) () (list (list 5 50) () ()))))

(define (key tree) (caar tree))


(define (lookup x set)
  (cond ((null? set) #f)
        ((= x (key set)) (car set))
        ((< x (key set))
         (lookup x (left-branch set)))
        ((> x (key set))
         (lookup x (right-branch set)))))

(print (lookup 5 tree1))

