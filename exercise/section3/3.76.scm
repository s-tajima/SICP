;= Question =============================================================================
;
; 問題 3.76
;
; Eva Lu Atorは問題3.75のLouisの解決法に意見を持っている. 
; 彼の書いたプログラムは, 平滑化の演算と零交差の抽出とが混ざっているので, 
; 部品化になっていない. 
; 
; 例えばAlyssaが入力信号を改善するよりよい方法を見つけても, 抽出部分は変更すべきではない. 
; 
; 入力としてストリームをとり,
; 各要素が入力ストリームの二つの連続する要素の平均であるストリームを
; 生じる手続きsmoothを書いてLouisを援助せよ. 
; 
; 次にsmoothをより部品化スタイルでの零交差検出器の実装の部品として使用せよ. 
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

(define (make-zero-crossings input-stream last-value smooth)
       (cons-stream (sign-change-detector (stream-car (smooth input-stream)) last-value)
                    (make-zero-crossings (stream-cdr input-stream)
                                         (stream-car (smooth input-stream)) smooth)))

(define (smooth stream)
  (cons-stream (/ (+ (stream-car stream) (car (stream-cdr stream))) 2)
               (smooth (stream-cdr stream))))

;= Test =================================================================================

(stream-head (smooth integers) 10)

(print)

(define zero-crossings (make-zero-crossings sense-data 0 smooth))
(stream-head zero-crossings 10)

