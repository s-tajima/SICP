;= Question =============================================================================
;問題 2.17
;
;与えられた(空でない)リストの最後の要素だけからなるリストを返す手続き last-pairを定義せよ:
;
;(last-pair (list 23 72 149 34))
;(34)
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

(define (last-pair l)
  (list (list-ref l (- (length l) 1))))

(print (last-pair (list 23 72 149 34)))
