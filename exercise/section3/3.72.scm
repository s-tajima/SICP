;= Question =============================================================================
;  
; 問題 3.71
; 
; 二通り以上の二つの立方数の和で表される数を, 
; 数学者Srinivasa Ramanujanを記念しRamanujan数(Ramanujan number)という.
; 
; 70 対の順序づけられたストリームは, こういう数を計算する問題に対する優美な解を提供する. 
; 二通りの方法で二つの立方数の和として書ける数を見つけるには, 
; 和i3 + j3に従って順序づけられた整数の対(i, j)の
; ストリームを生成するだけでよい(問題3.70参照). 
; そして同じ重みで二つ連続する対をストリームから探す. 
; Ramanujan数を生成する手続きを書け. 
; 最初のそういう数は1729である. 次の5個は何か.
; 
;= Prepared =============================================================================

(load "./stream.scm")

;= Answer ===============================================================================

(define (merge-weighted pairs1 pairs2 weight)
  (cond ((stream-null? (stream-car pairs1)) pairs2)
        ((stream-null? (stream-car pairs2)) pairs1)
        (else
          (let ((p1car (stream-car pairs1))
                (p2car (stream-car pairs2)))
               (if (< (weight p1car) (weight p2car))
                   (cons-stream p1car (merge-weighted pairs2 (stream-cdr pairs1) weight))
                   (cons-stream p2car (merge-weighted pairs1 (stream-cdr pairs2) weight)))))))

(define (weighted-pairs s t weight)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (merge-weighted
      (stream-map (lambda (x) (list (stream-car s) x))
                  (stream-cdr t))
      (weighted-pairs (stream-cdr s) (stream-cdr t) weight)
      weight)))

(define (squair-add pair)
  (let ((i (car pair))
        (j (cadr pair)))
       (+ (* i i) (* j j))))


(define (filter pairs)
  (let ((pair1 (stream-car pairs))
        (pair2 (car (stream-cdr pairs)))
        (pair3 (car (stream-cdr (stream-cdr pairs)))))
       (if (and (= (squair-add pair1) (squair-add pair2)) (= (squair-add pair1) (squair-add pair3)))
           (cons-stream (squair-add pair1) (filter (stream-cdr pairs)))
           (filter (stream-cdr pairs)))))

;= Test =================================================================================

(define p (weighted-pairs integers integers squair-add))

(stream-head (filter p) 10)
