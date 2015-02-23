;= Question =============================================================================
;
; 問題3.4
; 
; 問題3.3のmake-accountを, もう一つの局所状態変数を加えるように修正し, 
; 口座が不正なパスワードで連続七回アクセスされると, 
; 手続き call-the-copsを起動するようにせよ.
; 
;= Prepared =============================================================================


;= Answer ===============================================================================

(define (make-account balance password)
  (define num 0)
  (define (withdraw amount)
    (reset-num)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (reset-num)
    (set! balance (+ balance amount))
    balance)
  (define (incorrect-password amount)
    (begin (set! num (+ num 1))
           (if (< 7 num) call-the-cops "Incorrect password")))
  (define call-the-cops "call-the-cops")
  (define (reset-num) (set! num 0))
  (define (dispatch pass m)
    (cond ((not (eq? pass password)) incorrect-password)
          ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       m))))
  dispatch)

(define acc (make-account 100 'secret-password))
(print ((acc 'secret-password 'withdraw) 10))
(print ((acc 'secret-passwor 'withdraw) 10))
(print ((acc 'secret-passwor 'withdraw) 10))
(print ((acc 'secret-passwor 'withdraw) 10))
(print ((acc 'secret-passwor 'withdraw) 10))
(print ((acc 'secret-passwor 'withdraw) 10))
(print ((acc 'secret-passwor 'withdraw) 10))
(print ((acc 'secret-passwor 'withdraw) 10))
(print ((acc 'secret-passwor 'withdraw) 10))
(print ((acc 'secret-password 'withdraw) 10))

(print ((acc 'secret-passwor 'withdraw) 10))
(print ((acc 'secret-passwor 'withdraw) 10))
(print ((acc 'secret-passwor 'withdraw) 10))
(print ((acc 'secret-passwor 'withdraw) 10))
(print ((acc 'secret-passwor 'withdraw) 10))
(print ((acc 'secret-passwor 'withdraw) 10))
(print ((acc 'secret-passwor 'withdraw) 10))
(print ((acc 'secret-password 'withdraw) 10))
(print ((acc 'secret-passwor 'withdraw) 10))
