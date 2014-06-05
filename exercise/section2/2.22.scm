;= Question =============================================================================
; 問題 2.22
; 
; Louis Reasonerは問題2.21の初めのsquare-listを書き直し, 
; 反復プロセスを生成するようにした:
; 
; (define (square-list items)
;   (define (iter things answer)
;     (if (null? things)
;         answer
;         (iter (cdr things) 
;               (cons (square (car things))
;                     answer))))
;   (iter items nil))
; 
; 困ったことにこのsquare-listの定義は, 考えていたのと逆の順に答のリストを作った. 
; なぜか.
; 
;    Louisはconsの引数を交換して虫をとろうとした:
; 
; (define (square-list items)
;   (define (iter things answer)
;     (if (null? things)
;         answer
;         (iter (cdr things)
;               (cons answer
;                     (square (car things))))))
;   (iter items nil))
; 
; これも動かなかった. なぜか. ;
;
;= Prepared =============================================================================


;= Answer ===============================================================================
(define (square x) (* x x))


(define (square-list-1 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things) 
              (cons (square (car things))
                    answer))))
  (iter items ()))

(print (square-list-1 (list 1 3 5 7)))


(define (square-list-2 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items ()))

(print (square-list-2 (list 1 3 5 7)))

(define (square-list-3 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (append answer
                    (list (square (car things)))))))
  (iter items ()))

(print (square-list-3 (list 1 3 5 7)))
