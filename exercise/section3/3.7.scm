;= Question =============================================================================
;
; 問題 3.7
; 
; 問題3.3で述べたパスワードの修正つきmake-accountで
; 作り出した銀行口座オブジェクトを考えよう. 
; この銀行システムは共同口座を作る機能を要求した. これを行う手続き make-jointを定義せよ. 
; make-jointは三つの引数をとる. 
; 第一はパスワードで保護された口座. 
; 第二引数はmake-jointが働くためには口座を定義した時のパスワードと
; 合っていなければならない. 
; 第三引数は新しいパスワードである. 
; make-jointは新しいパスワードを使った元の口座への追加のアクセスを作り出す. 
; 例えば, peter-accがパスワード open-sesameを使った銀行口座なら,
; 
; (define paul-acc
;   (make-joint peter-acc 'open-sesame 'rosebud))
; 
; は名前paul-accとパスワードrosebudを使ってpeter-accでの取引きを許す. 
; この新機能を満すため, 問題3.3の解を修正してよい.
; 
;= Prepared =============================================================================


;= Answer ===============================================================================

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (incorrect-password amount)
    "Incorrect password")
  (define (dispatch pass m)
    (cond ((not (eq? pass password)) incorrect-password)
          ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       m))))
  dispatch)

(define (make-joint account account-password new-password)
  (define (dispatch password m)
    (if (eq? password new-password)
        (account account-password m)
        (error "Incorrect password" account-password)))
  dispatch)

(define peter-acc (make-account 100 'open-sesame))
(print ((peter-acc 'open-sesame 'withdraw) 20))
(print ((peter-acc 'secret-passwor 'withdraw) 20))
(print ((peter-acc 'open-sesame 'withdraw) 10))
(define paul-acc (make-joint peter-acc 'open-sesame 'rosebud))
(print ((paul-acc 'rosebud 'withdraw) 10))
(print ((peter-acc 'open-sesame 'withdraw) 10))
(define test-acc (make-joint paul-acc 'rosebud 'test))
(print ((test-acc 'test 'withdraw) 10))

;= Test =================================================================================

