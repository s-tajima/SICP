;= Question =============================================================================
;  
; 問題3.69

; 三つの無限ストリームS, T および Uを取り, 
; i ≤ j ≤ kなる三つ組(Si, Tj, Uk)のストリームを生じる手続きtriplesを書け. 
; triplesを使い, 正の整数の Pythagoras三つ組, つまりi ≤ jでi2 + j2 = k2である, 
; すべての三つ組(i, j, k)のストリームを生成せよ.
; 
;= Prepared =============================================================================

(load "./stream.scm")


(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

;= Answer ===============================================================================

(define (pairs s t)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (interleave
      (stream-map (lambda (x) (list (stream-car s) x))
                  (stream-cdr t))
      (pairs (stream-cdr s) (stream-cdr t)))))

(define (triples s t u)
  (cons-stream
    (list (stream-car s) (stream-car t) (stream-car u))
    (interleave
        (stream-map (lambda (x) (append (list (stream-car s)) x)) (pairs t (stream-cdr u)))
      (triples (stream-cdr s) (stream-cdr t) (stream-cdr u)))))


;= Test =================================================================================

;(stream-head  (pairs integers integers) 10)
;(stream-head  (triples integers integers integers) 100)

(define (pythagoras stream)
  (stream-filter
    (lambda (stream) 
            (let ((s (car stream)) 
                  (t (car (cdr stream)))
                  (u (car (cdr (cdr stream)))))
                 (= (+ (* s s) (* t t)) (* u u)))) 
    stream))

(stream-head (pythagoras (triples integers integers integers)) 3)


