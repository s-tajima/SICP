;= Question =============================================================================
;
; 問題 3.18
; 
; リストを調べ, そこに循環が含まれるか
; (つまりcdrを順にとってリストの終りを見つけようとするプログラムが無限ループに入るか)
; を決める手続きを書け. 問題3.13はそういうリストを構成した.
;
;= Prepared =============================================================================

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((eq? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

;= Answer ===============================================================================

(define (loop? items)
  (define walks '())
  (define (has-circulate? x)
    (if (memq x walks)
        #t
        (begin (set! walks (cons x walks)) #f)))
  (define (circulate?-iter i)
    (if (not (pair? i))
        #f
        (if (has-circulate? (car i))
            #t
            (circulate?-iter (cdr i)))))
  (circulate?-iter items))

(define x (list 'a 'b))
(define z1 (cons x x))
(define z2 (make-cycle (list 'a 'b 'c)))

(print (loop? z1))
(print (loop? z2))



;= Test =================================================================================

(define x (cons 1 2))
(define z (cons x x))
(set-car! (cdr z) 17)

(print (car x))

