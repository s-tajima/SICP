;= Question =============================================================================
;問題 2.18
;
;引数としてリストをとり, 同じ要素の逆順のリストを返す手続き reverseを定義せよ:
;
;(reverse (list 1 4 9 16 25))
;(25 16 9 4 1)
;
;
;= Prepared =============================================================================

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

;= Answer ================================================================================
(define (my-reverse l)
  (append
    (if (= (length l) 1) (list) (my-reverse (cdr l)))
    (list (car l))))

(print (my-reverse (list 1 2 3 4)))



