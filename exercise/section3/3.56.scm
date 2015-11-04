;= Question =============================================================================
;  
; 問題 3.56
; 
; R. Hammingが初めて提起した有名な問題は, 繰返しなしで大きくなる順に 
; 2, 3, 5以外の素数の因子を持たない整数を数え上げるものである. 
; 
; 明白な方法の一つは, 
; 単に各整数を次々と2, 3, 5以外の因子があるかどうかテストすることである. 
; しかしこれは整数が大きくなると, 要求を満す数がどんどん少くなるので, 非効率である. 
; 
; 別の方法として, 要求された数のストリームをSとし, それにつき次の事実に注意する.
; 
; •Sは1から始まる.
; •(scale-stream S 2)の要素はSの要素である.
; •(scale-stream S 3)と(scale-stream S 5)についても同様である.
; •これらはSの要素のすべてである.
; 
; これらのことから要素を混ぜ合せなければならない. 
; そのため, 二つの順序づけられたストリームを, 
; 繰返しなしに一つの順序づけられたストリームに混ぜ合せる手続きmergeを定義する:
; 
; (define (merge s1 s2)
;   (cond ((stream-null? s1) s2)
;         ((stream-null? s2) s1)
;         (else
;          (let ((s1car (stream-car s1))
;                (s2car (stream-car s2)))
;            (cond ((< s1car s2car)
;                   (cons-stream s1car (merge (stream-cdr s1) s2)))
;                  ((> s1car s2car)
;                   (cons-stream s2car (merge s1 (stream-cdr s2))))
;                  (else
;                   (cons-stream s1car
;                                (merge (stream-cdr s1)
;                                       (stream-cdr s2)))))))))
; 
; すると要求されたストリームはmergeを使い, 次のように構成出来る.
; 
; (define S (cons-stream 1 (merge ⟨??⟩ ⟨??⟩)))
; 
; 上の⟨??⟩ で記された場所の欠けた式を補え. 

;= Prepared =============================================================================

(load "./stream.scm")

(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons-stream s1car (merge (stream-cdr s1) s2)))
                 ((> s1car s2car)
                  (cons-stream s2car (merge s1 (stream-cdr s2))))
                 (else
                  (cons-stream s1car
                               (merge (stream-cdr s1)
                                      (stream-cdr s2)))))))))

;= Answer ===============================================================================

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define integers2 (scale-stream integers 2))
(define integers3 (scale-stream integers 3)) 
(define integers5 (scale-stream integers 5)) 

(define S (cons-stream 1 (merge (scale-stream S 2) (merge (scale-stream S 3) (scale-stream S 5)))))


;= Test =================================================================================

(print (stream-ref S 0))
(display-stream S)
