;= Question =============================================================================
;  
; 問題 3.70
; 
; アドホックな混合せの結果の順でなく,
; 対がある有用な順で現れるようにストリームが出来ると嬉しい. 整数の一つの対が,
; 他の対「より小さい」という方法が定義出来れば,
; 問題3.56のmerge手続きに似た技法を使うことが出来る. 
; その一つの方法は「重み関数」W(i, j)を定義し,
; W(i1, j1) < W(i2, j2)なら, (i1, j1)は(i2, j2)より小さいと規定することである.
; merge-weightedはもう一つの引数weightを取る他は,
; mergeに似た手続きmerge-weightedを書け. 
; 
; weight は対の重みを計算する手続きで, 
; 要素が結果の混ぜ合せたストリームに現れる順を決めるのに使う
; 
; これを使い, pairsを, 二つのストリームと重み関数を計算する手続きをとり,
; 重みに従って順序づけられた対のストリームを生成する手続きweighted-pairsに一般化し, 
; その手続きを使って
; 
; a. 和i + jに従って順序づけられた, i ≤ jなる正の整数の対(i, j)のすべてのストリーム
; 
; b. 和2i + 3j + 5ij に従って順序づけられた, i ≤ jで, 
;    iもjも2, 3, 5で割り切れない正の整数の対(i, j)のすべてのストリーム
; 
; を生成せよ. 
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

(define (add-pairs-weight pair)
  (+ (car pair) (cadr pair)))

(define (add-pairs-weight-2 pair)
  (+ (* 2 (car pair)) (* 3 (cadr pair)) (* 5 (car pair) (cadr pair))))



;= Test =================================================================================

(define p (weighted-pairs integers integers add-pairs-weight))
(stream-head p 20)

(print)

(define integers-no-remainder-2-3-5
  (stream-filter (lambda (x)
                         (not (or (= 0 (remainder x 2))
                                  (= 0 (remainder x 3))
                                  (= 0 (remainder x 5)))))
                 integers))

(define q (weighted-pairs integers-no-remainder-2-3-5 integers-no-remainder-2-3-5 add-pairs-weight-2))
(stream-head q 20)
