;= Question =============================================================================
;
; 問題 3.13
;
; 問題3.12で定義したlast-pair手続きを使う次のmake-cycle手続きを考える:
; 
; (define (make-cycle x)
;   (set-cdr! (last-pair x) x)
;   x)
;     
; その時,
;     
; (define z (make-cycle (list 'a 'b 'c)))
;     
; で作り出す構造zを示す箱とポインタ図を描け. 
; (last-pair z)を計算しようとすると何が起きるか. 
;
;= Prepared =============================================================================
 
(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

;= Answer ===============================================================================

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define z (make-cycle (list 'a 'b 'c)))

;(print z)
;(print (last-pair z))

(print (car (cons 1 2)))
(print (cdr (cons 1 2)))

;= Test =================================================================================

