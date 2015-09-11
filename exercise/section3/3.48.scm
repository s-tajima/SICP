;= Question =============================================================================
;  
; 問題 3.48
; 
; 上に述べたデッドロック回避法(つまり口座に番号をつけ, 
; プロセスはより小さい番号の方の口座を先に獲得しようとする.)
; が交換問題でデッドロックを回避する理由を詳しく説明せよ. 
; この方法を採用するようserialized-exchangeを書き直せ. 
; (make-accountも書き直し, 各口座は番号と共に作り出され, 
; その番号は適切なメッセージを送ってアクセス出来るようにしなければならない.) 
;
;= Prepared =============================================================================

(define (make-serializer)
  (let ((mutex (make-mutex)))
       (lambda (p)
               (define (serialized-p . args)
                 (mutex 'acquire)
                 (let ((val (apply p args)))
                      (mutex 'release)
                      val))
               serialized-p)))

(define (make-mutex)
  (let ((cell (list #f)))            
       (define (the-mutex m)
         (cond ((eq? m 'acquire)
                (if (test-and-set! cell)
                    (the-mutex 'acquire))) ; retry
               ((eq? m 'release) (clear! cell))))
       the-mutex))

(define (clear! cell)
  (set-car! cell #f))

(define (test-and-set! cell)
  (if (car cell)
      #t
      (begin (set-car! cell #t)
             #f)))

(define (exchange account1 account2)
  (let ((difference (- (account1 'balance)
                       (account2 'balance))))
       ((account1 'withdraw) difference)
       ((account2 'deposit) difference)))


;
;= Answer ===============================================================================

(define (make-account-and-serializer balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((balance-serializer (make-serializer))
        (id (id-generator)))
       (define (dispatch m)
         (cond ((eq? m 'withdraw) withdraw)
               ((eq? m 'deposit) deposit)
               ((eq? m 'balance) balance)
               ((eq? m 'serializer) balance-serializer)
               ((eq? m 'id) id)
               (else (error "Unknown request -- MAKE-ACCOUNT"
                            m))))
       dispatch))

(define (make-id-generator)
  (let ((id 0))
  (lambda ()
          (begin
            (set! id (+ id 1))
            id))))

(define id-generator (make-id-generator))

(define (serialized-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer))
        (id1 (account1 'id)) 
        (id2 (account2 'id)))
       (if (> id1 id2)
            ((serializer1 (serializer2 exchange)) account1 account2)
            ((serializer2 (serializer1 exchange)) account1 account2))))

;= Test =================================================================================

(define a1 (make-account-and-serializer 15))
(print (a1 'balance))
(print (a1 'id))
(define a2 (make-account-and-serializer 20))
(print (a2 'balance))
(print (a2 'id))

(serialized-exchange a1 a2)
(print (a1 'balance))
(print (a2 'balance))

(serialized-exchange a2 a1)
(print (a1 'balance))
(print (a2 'balance))


