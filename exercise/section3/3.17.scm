;= Question =============================================================================
;
; 問題 3.17
; 
; 問題3.16のcount-pairsで, 任意の構造の異る対の個数を返す正しい解を考えよ. 
; (ヒント: どの対が既に数えられたかを覚えておくのに使う
;  補助のデータ構造を補正しながら構造を渡り歩け.)
;
;= Prepared =============================================================================

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((eq? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

;= Answer ===============================================================================

(define (count-pairs-fixed pair)
  (define walked-pairs (list))
  (define (walk-pair pair)
    (if (and (pair? pair)
             (not (element-of-set? pair walked-pairs)))
        (begin (set! walked-pairs (append walked-pairs (list pair)))
               (walk-pair (car pair))
               (walk-pair (cdr pair)))
        ())) 
  (walk-pair pair)
  (length walked-pairs))

(define x (list 'a 'b))
(define z1 (cons x x))


(print (count-pairs-fixed z1))


;= Test =================================================================================

