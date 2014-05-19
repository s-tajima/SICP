;= Question =============================================================================
; 問題 2.19
;
; 1.2.2節の両替の計算のプログラムを考えよう. 
; プログラムの使っている通貨が簡単に変えられると, 
; 例えば英国の1ポンドの両替の出し方の数を計算することも出来, 嬉しい. 
; あのプログラムを書いた時, 通貨の知識は一部は手続きfirst-denominationに, 
; また一部は手続きcount-changeに分散されていた. 
; (これが米国に5種類の硬貨のあることを知っていた.) 
; 
; 両替をするのに使う硬貨のリストを与えることが出来れば, さらに嬉しいに違いない.
; 
; 手続きccを書き直し, その第二引数を, どの硬貨を使うかでなく, 
; 使う硬貨の値のリストとする. 
; 硬貨の種類を定義するリストが作れる.
; 
;   (define us-coins (list 50 25 10 5 1))
; 
;   (define uk-coins (list 100 50 20 10 5 2 1 0.5))
; 
; ccを次のように呼び出す:
; 
;    (cc 100 us-coins)
;    292
; 
; こうするには, プログラムccを多少変える必要がある. 
; 同じ恰好をしているが, 第二引数のアクセスが次のように変る:
; 
;   (define (cc amount coin-values)
;     (cond ((= amount 0) 1)
;           ((or (< amount 0) (no-more? coin-values)) 0)
;           (else
;             (+ (cc amount
;                    (except-first-denomination coin-values))
;                (cc (- amount
;                       (first-denomination coin-values))
;                    coin-values)))))
; 
; 手続きfirst-denomination, except-first-denominationおよびno-more?を
; リスト構造の基本的演算を使って定義せよ. 
; リスト coin-valuesの順は, ccの答に影響があるか. なぜか. 
;
;= Prepared =============================================================================
(define us-coins (list 50 25 10 5 1))
 
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
          (+ (cc amount
                 (except-first-denomination coin-values))
             (cc (- amount
                    (first-denomination coin-values))
                 coin-values)))))

;= Answer ================================================================================
(define (first-denomination coin-values)
  (car coin-values))

(define (except-first-denomination coin-values)
  (cdr coin-values))

(define (no-more? coin-values)
  (null? coin-values))

(print (cc 100 us-coins))
