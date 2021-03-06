;= Question =============================================================================
;
; 問題3.7
;
; 1.1.3節で評価モデルを定義した時, 式の評価の第一歩はその部分式を評価することだといった. 
; しかし部分式を評価する順(例えば左から右か右から左か) は規定しなかった. 
; 代入を取り入れると, 手続きへの引数が評価される順は結果に違いを生じるかも知れない. 
; 単純な手続きfを定義し, (+ (f 0) (f 1))が,
; +の引数を左から右へ評価すると0を返し, 右から左へ評価すると1を返すようにせよ.
; 
;= Prepared =============================================================================


;= Answer ===============================================================================

(define f
  (let ((y 1))
  (lambda (x) (begin (set! y (* y x)) y))))

;(print (+ (f 0) (f 1)))
(print (+ (f 1) (f 0)))

;= Test =================================================================================

