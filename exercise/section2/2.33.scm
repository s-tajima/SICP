;= Question =============================================================================
;
;問題 2.33
;
;リスト操作の基本演算の, アキュムレーションとしての定義が以下にある. 
;欠けた部分を補って完成せよ.
;
; (define (map p sequence)
;   (accumulate (lambda (x y) ⟨??⟩) nil sequence))
; 
; (define (append seq1 seq2)
;   (accumulate cons ⟨??⟩ ⟨??⟩))
; 
; (define (length sequence)
;   (accumulate ⟨??⟩ 0 sequence))
;
;= Prepared =============================================================================

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (square x) (* x x))

;= Answer ===============================================================================

(define (mymap p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) () sequence))

(print (map   square (list 1 2 3 4)))
(print (mymap square (list 1 2 3 4)))

(define (myappend seq1 seq2)
  (accumulate cons seq2 seq1))

(print (append   (list 1 2 3 4) (list 5 6 7 8)))
(print (myappend (list 1 2 3 4) (list 5 6 7 8)))

(define (mylength sequence)
  (accumulate (lambda (x y) (+ y 1)) 0 sequence))

(print (length   (list 1 2 4 8 16)))
(print (mylength (list 1 2 4 8 16)))
