;= Question =============================================================================
;
; 問題 3.80
; 
; 直列RLC回路(series RLC circuit)は, 
; 図3.36に示すように直列に接続した抵抗, インダクタ, キャパシタからなる. 
; 
; R, LおよびCを抵抗値, 誘導係数および容量とすると, 三つの素子の電圧(v), 電流(i)の関係は式
; 
; で表せ, 回路の接続は関係
; 
; を要請する. 
; 
; これらの式を組み合せると
; (キャパシタの電圧vCとインダクタの電流iLでまとめた)回路の状態は,
; 一対の微分方程式
; 
; 
; で表せる. 
; 
; この微分方程式のシステムを表現する信号流れのダイアグラムを図3.37に示す.
; 
; 
; 図3.36 直列RLC回路
; 図3.37 直列RLC回路の解のための信号流れのダイアグラム
; 
; 
; 引数として回路のパラメタR, LおよびCと, 時間増分dtをとる手続きRLCを書け.
; 問題3.73のRC手続きと同様に,
; RLCは状態変数vC0とiL0の初期値をとり,
; 状態vCとiLのストリームの対(consを使う)を生じる手続きを生じるものとする.
; 
; このRLCを使って,
; R=1オーム, C=0.2ファラド, L=1ヘンリ, dt=0.1秒, 初期値iL0=0アンペア, 
; vC0=10ボルトの直列RLC回路の振舞いをモデル化する一対のストリームを生成せよ. 
;
;= Prepared =============================================================================

(load "./stream.scm")


(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define (integral delayed-integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (let ((integrand (force delayed-integrand)))
                      (add-streams (scale-stream integrand dt)
                                   int))))
  int)

(define (rlc r c l dt)
  (define (proc vc0 il0)
    (define vc (integral (delay dvc) vc0 dt))
    (define il (integral (delay dil) il0 dt))
    (define dvc (scale-stream il (/ -1 c)))
    (define dil (add-streams (scale-stream vc (/ 1 l)) (scale-stream il (- (/ r l)))))
    (cons vc il))
  proc)

;= Answer ===============================================================================

;= Test =================================================================================

(define circuit (rlc 1 0.2 1 0.1))
(define answer (circuit 10 0))
(stream-head (car answer) 10)
(print)
(stream-head (cdr answer) 10)
