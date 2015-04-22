;= Question =============================================================================
;
; 問題 3.12
; 
; リストを連結する次の手続きは2.2.1節で説明した:
; 
; (define (append x y)
;   (if (null? x)
;       y
;       (cons (car x) (append (cdr x) y))))
; 
; appendはxの要素を次々とyへconsして新しいリストを作る. 
; 手続きappend!はappendに似ているが, 構成子ではなく変更子である. 
; xの最後の対を, そのcdrがyになるように修正し, リストを一緒に張り合せて連結する. 
; (空のxについてappend!を呼び出すとエラーになる.)
; 
; (define (append! x y)
;   (set-cdr! (last-pair x) y)
;   x)
; 
; last-pairはその引数の最後の対を返す手続きである:
; 
; 
; (define (last-pair x)
;   (if (null? (cdr x))
;       x
;       (last-pair (cdr x))))
; 
; 次の対話を考える.
; 
; (define x (list 'a 'b))
; 
; (define y (list 'c 'd))
; 
; (define z (append x y))
; 
; z
; (a b c d)
; 
; (cdr x)
; ⟨応答⟩
; 
; (define w (append! x y))
; 
; w
; (a b c d)
; 
; (cdr x)
; ⟨応答⟩
; 
; 欠けている⟨応答⟩は何か. 箱とポインタの図を描いて説明せよ. 
; 
;= Prepared =============================================================================
 
(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)


(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

;= Answer ===============================================================================

(define x (list 'a 'b))

(define y (list 'c 'd))

(define z (append x y))

(print z)
(print (cdr x))

(define w (append! x y))

(print w)
(print (cdr x))

;= Test =================================================================================

