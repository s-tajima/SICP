;= Question =============================================================================
;
; 問題 3.81
; 
; 問題3.6は乱数発生器を一般化し, 
; 乱数の並びをリセットして「乱」数の反復可能な並びが作れるようにすることを論じた.
; 新しい乱数をgenerateするか, 
; 規定した値に並びをresetするかの要求の入力ストリームに操作して, 
; 望みの乱数が作れるこの発生器のストリーム形式化を作れ. 解に代入を使ってはいけない. 
;
;= Prepared =============================================================================

(load "./stream.scm")

(define random-init 1)
(define (rand-update x)
  (remainder (+ x 1812433253) 4294967296))

(define (rand input-stream random-init)
  (define random-stream
    (if (stream-null? input-stream)
        the-empty-stream
        (let ((request (stream-car input-stream)))
             (cons-stream
               (cond ((eq? request 'generate) (rand-update random-init))
                     ((number? request) (rand-update request))
                     (else (error "Unknown request --- RAND" request)))
               (rand (stream-cdr input-stream) (stream-car random-stream))))))
  random-stream)


;= Answer ===============================================================================

(define request-stream
  (cons-stream 100
               (cons-stream 'generate
                            (cons-stream 'generate
                                         (cons-stream 100
                                                      (cons-stream 'generate
                                                                   (cons-stream 'generate
                                                                                the-empty-stream)))))))

(print (stream-cdr (rand request-stream 2)))
(print (stream-cdr (stream-cdr (rand request-stream 2))))

;= Test =================================================================================
