;= Question =============================================================================
;
; 問題 2.37
; 
; ベクタv=(vi)を数の並びで, 
; マトリクスm=(mij)をベクタ(マトリクスの行)の並びで表現するとしよう. 
; 例えばマトリクス
; 
; は並び((1 2 3 4) (4 5 6 6) (6 7 8 9))と表現する. 
; この表現を使えば, 
; 並びの演算を使ってマトリクスやベクタの基本演算を簡潔に表すことが出来る. 
; これらの演算(大抵の行列演算の教科書に書いてある.)は次の通り:
; 
; (dot-product v w)       総和∑i viwiを返す;
; (matrix-*-vector m v)   ti =∑jmijvjであるようなベクタtを返す;
; (matrix-*-matrix m n)   pij=∑k miknkjであるようなマトリックスpを返す;
; (transpose m)           nij=mjiであるようなマトリックスnを返す. ;
; 
; 内積は
; 
; (define (dot-product v w)
;   (accumulate + 0 (map * v w)))
; 
; と定義出来る.17 他のマトリクス演算を計算する次の手続きの欠けた式を補え.
; (手続きaccumulate-nは問題2.36で定義してある.)
; 
; (define (matrix-*-vector m v)
;   (map ⟨??⟩ m))
; 
; (define (transpose mat)
;   (accumulate-n ⟨??⟩ ⟨??⟩ mat))
; 
; (define (matrix-*-matrix m n)
;   (let ((cols (transpose n)))
;        (map ⟨??⟩ m)))
; 
;= Prepared =============================================================================

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      ()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))


(define matrix
  (list (list 1 1 1)
        (list 2 2 2)
        (list 3 3 3)
        ))

(define matrix2
  (list (list 1 2 3)
        (list 4 5 6)
        (list 6 7 8)
        ))

(define vect
  (list 1 2 3))

;= Answer ===============================================================================

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(print (dot-product vect vect))

(define (matrix-*-vector m v)
  (map (lambda (m) (dot-product v m)) m))

(print (matrix-*-vector matrix vect))

(define (transpose mat)
  (accumulate-n cons () mat))

(print (transpose matrix))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
      (map (lambda (m) (matrix-*-vector cols m) ) m)))
 
(print (matrix-*-matrix matrix matrix2))
