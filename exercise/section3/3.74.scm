;= Question =============================================================================
;
; 問題 3.74
;
; Alyssa P. Hackerは物理的検出器からの信号を処理するシステムを設計している. 
; 彼女が作ろうとしている一つの重要な機能は, 
; 入力信号の零交差 (zero crossing)を記述する信号である. 
; 
; 
; つまり結果の信号は, 入力信号が負から正に変った時は+1,
; 入力信号が正から負へ変った時は-1, それ以外は0とする. 
; (0入力の符号は正と仮定する.) 
; 
; 例えば典型的な入力信号と対応する零交差信号は
; 
; ... 1  2  1.5  1  0.5  -0.1  -2  -3  -2  -0.5  0.2  3  4 ...
; ...  0  0    0  0    0     -1  0   0   0     0    1  0  0 ...
; 
; となる. Alyssaのシステムでは検出器からの信号はストリーム sense-dataで表現され, 
; ストリームzero-crossingsが対応する零交差のストリームである. 
; Alyssaはまず手続きsign-change-detectorを書いた. 
; 
; これは引数として二つの値をとり, それらの値の符号を比較し, 適切な0, 1または-1を生じる.
; 彼女はそれから零交差ストリームを次のように構成した.
; 
; (define (make-zero-crossings input-stream last-value)
;   (cons-stream
;     (sign-change-detector (stream-car input-stream) last-value)
;     (make-zero-crossings (stream-cdr input-stream)
;                          (stream-car input-stream))))
; 
; (define zero-crossings (make-zero-crossings sense-data 0))
; 
; Alyssaの上司のEva Lu Atorは通りかかり, 
; このプログラムは問題3.50のstream-mapの一般化した版を使う次のと大体同じだといった:
; 
; (define zero-crossings
;   (stream-map sign-change-detector sense-data ⟨expression⟩))
; 
; ⟨expression⟩を補ってプログラムを完成せよ.
;
;= Prepared =============================================================================

(load "./stream.scm")

;= Answer ===============================================================================

(define (sign-change-detector i l)
  (cond ((and (> 0 l) (<= 0 i)) 1)
        ((and (<= 0 l) (> 0 i)) -1)
        (else  0)))

(define (make-zero-crossings input-stream last-value)
  (cons-stream
    (sign-change-detector (stream-car input-stream) last-value)
    (make-zero-crossings (stream-cdr input-stream)
                         (stream-car input-stream))))

(define sense-data
  (stream-map (lambda (x) (sin x)) integers))


(define zero-crossings (make-zero-crossings sense-data 0))

(stream-head zero-crossings 10)

(print)

(define zero-crossings-2
  (stream-map sign-change-detector sense-data (cons-stream 0 sense-data)))

(stream-head zero-crossings-2 10)

;= Test =================================================================================

