;= Question =============================================================================
;
; 問題 3.14
;
; 次の手続きは分り難いが極めて有用である.
; 
; (define (mystery x)
;   (define (loop x y)
;     (if (null? x)
;         y
;         (let ((temp (cdr x)))
;              (set-cdr! x y)
;              (loop temp x))))
;   (loop x '()))
; 
; loopは, 次の行のset-cdr!がcdrを壊すので, 
; 「一時的」変数tempにxのcdrの前の値をとっておく. 
; 一般にmysteryが何をするか説明せよ. 
; vを(define v (list 'a 'b 'c 'd)) で定義したとする. 
; vが束縛されているリストを表現する箱とポインタ図を描け. 
; (define w (mystery v))を評価するとしよう. 
; この式を評価した後の構造vとwを示す箱とポインタ図を描け. 
; vとwの値として何が印字されるか. 
;
;= Prepared =============================================================================

(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
             (set-cdr! x y)
             (loop temp x))))
  (loop x '()))
 

;= Answer ===============================================================================

(define v (list 'a 'b 'c 'd))
(print v)

(define w (mystery v))

(print v)
(print w)


;= Test =================================================================================

