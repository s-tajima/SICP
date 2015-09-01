;= Question =============================================================================
;
; 問題 3.25
;
; 一次元, 二次元を一般化し, 値が任意個数のキーで格納され, 
; 値が異ればキーの個数も異るかも知れぬ表を実装する方法を示せ. 
; lookupと insert! の手続きは, 
; 入力として表にアクセスするのに使うキーのリストをとるものとする.
;
;= Prepared =============================================================================

(define (assoc key records)
  (cond ((null? records) #f)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

;= Answer ===============================================================================

(define (make-table)
  (let ((local-table (list '*table*)))

    (define (lookup key-list)
      (lookup-iter key-list local-table))
    (define (lookup-iter key-list local-table)
      (if (null? key-list)
          #f
          (let ((subtable (assoc (car key-list) (cdr local-table))))
               (if subtable
                   (if (null? (cdr key-list))
                       (cdr subtable)
                       (lookup-iter (cdr key-list) subtable))
                   #f))))


    (define (insert! key-list value)
      (insert-iter! key-list value local-table))

    (define (insert-iter! key-list value local-table)
      (if (null? key-list)
        #f
        (let ((subtable (assoc (car key-list) (cdr local-table))))
             (if subtable 
                (if (null? (cdr key-list))
                    (set-cdr! subtable value)
                    (insert-iter! (cdr key-list) value subtable))
                (set-cdr! local-table
                    (cons (insert-iter key-list value)
                          (cdr local-table))))))
      'ok)

    (define (insert-iter key-list value)
      (if (null? (cdr key-list))
          (cons (car key-list) value)
          (list (car key-list) (insert-iter (cdr key-list) value))))


    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

;= Test =================================================================================

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

(print (put '(a aa abc) 1))
(print (put '(a aa) 2))
(print (get '(a aa abc)))
(print (get '(a aa)))

