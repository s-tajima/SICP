;= Question =============================================================================
;
; 問題 3.75
;
; 困ったことに問題3.74のAlyssaの零交差検出器は, 
; 検出器からの雑音の多い信号が疑似的零交差を生じるので, 不十分なことが分かった. 
; 
; ハードウェアの専門家のLem E. TweakitはAlyssaに零交差を抽出する前に信号を平滑化し, 
; 雑音を除去するよう忠告した. 
; 
; Alyssaはその忠告に従い, 
; 検出データの各値を直前の値と平均して構成した信号から零交差を取り出すことにした. 
; 彼女は助手のLouis Reasonerに問題を説明すると, 
; 彼はAlyssaのプログラムを次のように変更して, この考えを実装しようとした:
; 
; (define (make-zero-crossings input-stream last-value)
;   (let ((avpt (/ (+ (stream-car input-stream) last-value) 2)))
;        (cons-stream (sign-change-detector avpt last-value)
;                     (make-zero-crossings (stream-cdr input-stream)
;                                          avpt))))
; 
; これはAlyssaの計画を正しくは実装していない. 
; Louisが入れた虫を見つけ, プログラムの構造を変えずに修正せよ. 
; (ヒント: make-zero-crossingsの引数の個数を増やす必要がある.) 
;
;= Prepared =============================================================================

(load "./stream.scm")

(define (sign-change-detector i l)
  (cond ((and (> 0 l) (<= 0 i)) 1)
        ((and (<= 0 l) (> 0 i)) -1)
        (else  0)))

(define sense-data
  (stream-map (lambda (x) (sin x)) integers))

;= Answer ===============================================================================

(define (make-zero-crossings input-stream last-value last-avpt)
  (let ((avpt (/ (+ (stream-car input-stream) last-value) 2)))
       (cons-stream (sign-change-detector avpt last-value)
                    (make-zero-crossings (stream-cdr input-stream)
                                         (stream-car input-stream) avpt))))

;= Test =================================================================================

(define zero-crossings (make-zero-crossings sense-data 0 0))
(stream-head zero-crossings 10)

