;= Question =============================================================================
;
; 問題3.3
;
; make-accountを修正し, パスワードで保護された口座を作り出すようにせよ. 
; つまりmake-accountはもう一つの引数として記号をとる. 例えば
; 
; (define acc (make-account 100 'secret-password))
; 
; 結果の口座オブジェクトは, 口座を作り出した時のパスワードのついている要求だけを処理し, 
; それ以外は文句をいう:
; 
; ((acc 'secret-password 'withdraw) 40)
; 60
; 
; ((acc 'some-other-password 'deposit) 50)
; "Incorrect password"
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

(define acc (make-account 100 'secret-password))
(print ((acc 'secret-password 'withdraw) 40))
(print ((acc 'secret-passwor 'withdraw) 40))
(print ((acc 'secret-password 'withdraw) 40))
