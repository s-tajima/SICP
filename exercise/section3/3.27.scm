;= Question =============================================================================
;
; 問題 3.27
;
; メモ化(memoization)(またはテーブル化(tabulation))は
; 手続きに以前計算した値を局所表に記録させる技法である. 
; この技法はプログラムの効率に大きな差をつけることがある. 
; メモ化の手続きは表を維持し, そこに過去の呼出しの値を, 
; その値を生じた引数をキーとして格納する. 
; 
; メモ化手続きが値を計算するよう依頼されると, 
; まずその値がすでにそこに存在するか表を調べ, それがあればその値を返す. 
; それがなければ通常の方法で新しい値を計算し, これを表に格納する. 
; メモ化の例として, 1.2.2節のFibonacci数を計算する指数的プロセスを思い出そう:
; 
; (define (fib n)
;   (cond ((= n 0) 0)
;         ((= n 1) 1)
;         (else (+ (fib (- n 1))
;                  (fib (- n 2))))))
; 
; 同じ手続きのメモ化版は
; 
; (define memo-fib
;   (memoize (lambda (n)
;                    (cond ((= n 0) 0)
;                          ((= n 1) 1)
;                          (else (+ (memo-fib (- n 1))
;                                   (memo-fib (- n 2))))))))
; 
; で, そのメモ化手続きは
; 
; (define (memoize f)
;   (let ((table (make-table)))
;        (lambda (x)
;                (let ((previously-computed-result (lookup x table)))
;                     (or previously-computed-result
;                         (let ((result (f x)))
;                              (insert! x result table)
;                              result))))))
; 
; で定義する. (memo-fib 3)の計算を解析する環境の図を描け. 
; memo-fibがn番目のFibonacci数をnに比例したステップ数で計算出来る理由を説明せよ. 
; この方式はmemo-fibを単に(memoize fib)と定義してもやはり働くだろうか.
;
;= Prepared =============================================================================

(define (make-table)
  (let ((local-table (list '*table*)))
       (define (lookup key-1 key-2)
         (let ((subtable (assoc key-1 (cdr local-table))))
              (if subtable
                  (let ((record (assoc key-2 (cdr subtable))))
                       (if record
                           (cdr record)
                           false))
                  false)))
       (define (insert! key-1 key-2 value)
         (let ((subtable (assoc key-1 (cdr local-table))))
              (if subtable
                  (let ((record (assoc key-2 (cdr subtable))))
                       (if record
                           (set-cdr! record value)
                           (set-cdr! subtable
                                     (cons (cons key-2 value)
                                           (cdr subtable)))))
                  (set-cdr! local-table
                            (cons (list key-1
                                        (cons key-2 value))
                                  (cdr local-table)))))
         'ok)    
       (define (dispatch m)
         (cond ((eq? m 'lookup-proc) lookup)
               ((eq? m 'insert-proc!) insert!)
               (else (error "Unknown operation -- TABLE" m))))
       dispatch))

(define (fib n)
  (cond ((= n 0) 0)
          ((= n 1) 1)
                  (else (+ (fib (- n 1))
                                   (fib (- n 2))))))

(define (memoize f)
  (let ((table (make-table)))
       (lambda (x)
               (let ((previously-computed-result (lookup x table)))
                    (or previously-computed-result
                        (let ((result (f x)))
                             (insert! x result table)
                             result))))))

(define memo-fib
  (memoize (lambda (n)
                   (cond ((= n 0) 0)
                         ((= n 1) 1)
                         (else (+ (memo-fib (- n 1))
                                  (memo-fib (- n 2))))))))


;= Answer ===============================================================================

(print (memo-fib 10000))

;= Test =================================================================================

