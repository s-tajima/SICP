;= Question =============================================================================
;
; 問題 3.24
;
; 表の上の実装では, キーはその等価性を(assocの呼び出す) equal?を使ってテストする.
; これは常に適切なテストとは限らない.
; 例えば数値のキーを持つ表があり,探している数値と厳密に一致する必要はなく,
; ある許容範囲内にあればよいかも知れない.
; 表の構成子make-tableで引数としてキーの等価性のテストに使う
; same-key?手続きをとるものを設計せよ.
; make-tableは,
; 局所表の適切なlookupやinsert!の手続きへのアクセスに使うdispatch手続きを返すものとする.
;
;= Prepared =============================================================================

(define same-key?
  (lambda (key, record) (equal? key record)))

(define (make-table s-key?)
  (let ((local-table (list '*table*)))
    (define (myassoc key records)
      (cond ((null? records) #f)
            (s-key? key (caar records) (car records))
            (else (myassoc key (cdr records)))))

    (define (lookup key-1 key-2)
      (let ((subtable (myassoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (myassoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  #f))
            #f)))

    (define (insert! key-1 key-2 value)
      (let ((subtable (myassoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (myassoc key-2 (cdr subtable))))
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


;= Answer ===============================================================================

(define operation-table (make-table same-key?))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

(print (put 'a 'a 'value_aa))
(print (put 'a 'b 'value_ab))
(print (put 'a 'c 'value_ac))
(print (get 'a 'b))


;= Test =================================================================================

