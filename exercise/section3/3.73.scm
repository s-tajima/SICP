;= Question =============================================================================
;
; 問題 3.73
;
; 時系列での電流や電圧の値を表現するのにストリームを使い,
; 電気回路をモデル化出来る.
;
; 例えば, 抵抗値Rの抵抗と, 容量Cのキャパシタが直列の RC回路(RC circuit)があるとしよう.
; 注入電流iに対する回路の電圧応答vは,
; 図3.33の式で決り, その構造は付随する信号流れ図に示す.
; この回路をモデル化する手続きRCを書け.
; RCは入力としてR, Cとdtの値をとり,
; 入力として電流iを表現するストリームとキャパシタの電圧の初期値v0をとり,
; 出力として電圧vのストリームを返す手続きを返すものとする.
;
; 例えば(define RC1 (RC 5 1 0.5))を評価することで
; R = 5オーム, C = 1ファラド, 0.5秒の時間間隔のRC回路を
; モデル化するのにRCが使えなければならない.
;
; これは電流の時系列を表すストリームキャパシタの初期電圧をとり,
; 電圧の出力ストリームを生じる手続きとしてRC1を定義する.
;
;= Prepared =============================================================================

(load "./stream.scm")

;= Answer ===============================================================================

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (add-streams (scale-stream integrand dt)
                              int)))
  int)

(define (rc r c dt)
  (define (proc i v)
    (add-streams (scale-stream i r)
                 (integral (scale-stream i (/ 1 c)) v dt)))
  proc)

;= Test =================================================================================

(define RC1 (rc 5 1 0.5))
(stream-head (RC1 integers 1) 10)
