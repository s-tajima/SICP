;= Question =============================================================================
; 
; 問題 2.54
; 
; 二つのリストは, 同じ順に並んだ同じ要素を含む時, equal?であるという. 例えば
; 
; (equal? '(this is a list) '(this is a list))
; 
; は真である. しかし
; 
; (equal? '(this is a list) '(this (is a) list))
; 
; は偽である. より正確には, 基本となるeq?等価を使い, 
; aと bが記号であって両者がeq?であるか, 
; あるいは両者が, (car a) が(car b)にequal?であり, 
; (cdr a)が(cdr b)にequal?であるようなリストであると, 
; equal?を再帰的に定義出来る. 
; 
; この考えを使い, equal?を実装せよ.36
; 
;= Prepared =============================================================================

(equal? '(this is a list) '(this is a list))
(equal? '(this is a list) '(this (is a) list))

;= Answer ===============================================================================

(print (equal? '(this is a list) '(this is a list)))
(print (equal? '(this is a list) '(this (is a) list)))


(define (myequal? a b)
  (if (and (eq? a ()) (eq? b ())) 
      #t
      (and (eq? (car a) (car b)) (myequal? (cdr a) (cdr b)))))

(print (myequal? '(this is a list) '(this is a list)))
(print (myequal? '(this is a list) '(this (is a) list)))
