;= Question =============================================================================
;  
; 図3.27はn個の全加算器を繋げた 繰上り伝播加算器(ripple-carry adder)を示す.
; これは二つのnビット二進数を足す並列加算器の最も単純なものである.
; 
; 入力A1, A2, A3, ..., AnおよびB1, B2, B3, ..., Bn
; は足すべき二つの二進数である(各AkとBkは0か1).
; 
; 回路はnビットの和S1, S2, S3, ..., Snと, 加算の繰上りCを生成する.
; この回路を生成する手続きripple-carry-adderを書け.
; 
; 手続きは引数としてn個の回線の三つのリスト---それぞれAk, BkおよびSk---と, 
; もう一本の回線Cをとるものとする.
; 
; 繰上り伝播加算器の主な欠点は, 繰上り信号の伝るのを待たなければならないことである.
; nビットの繰上り伝播加算器から, 完全な出力が得られるまでの遅延は, 
; アンドゲート,オアゲートおよびインバータの遅延を使ってどの位か. 
;
;= Prepared =============================================================================

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
       (half-adder b c-in s c1)
       (half-adder a s sum c2)
       (or-gate c1 c2 c-out)
       'ok))


(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
       (or-gate a b d)
       (and-gate a b c)
       (inverter c e)
       (and-gate d e s)
       'ok))

;= Answer ===============================================================================


(define (ripple-carry-adder a b sum c-out)
  (define (ripple-carry-adder-iter a b sum c-in)
    (if (not (null? a))
        (let ((c-out (make-wire)))
             ((full-adder (car a) (car b) c-in sum c-out)
              (ripple-carry-adder-iter (cdr a) (cdr b) sum c-out)))
        'ok))
  (ripple-carry-adder-iter a b sum c-out))


;= Test =================================================================================

