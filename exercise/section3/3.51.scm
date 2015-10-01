;= Question =============================================================================
;  
; 問題 3.51
; 
; 遅延評価がよく見えるように, 引数を印字してからそれを返すだけの次の手続きを使おう.
; 
; (define (show x)
;   (display-line x)
;     x)
; 
; 次のそれぞれの式の評価に応じて, 解釈系は何を印字するか.59
; 
; (define x (stream-map show (stream-enumerate-interval 0 10)))
; 
; (stream-ref x 5)
; 
; (stream-ref x 7)
;
;= Prepared =============================================================================

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define stream-null? null?)
(define the-empty-stream '())

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))

(define-syntax cons-stream
               (syntax-rules ()
                             ((_ a b) (cons a (delay b)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))

(define stream-null? null?)

(define (stream-car stream) (car stream))

(define (stream-cdr stream) (force (cdr stream)))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
        low
        (stream-enumerate-interval (+ low 1) high))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))


(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (show x)
  (display-line x)
    x)

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
        (apply proc (map stream-car argstreams))
        (apply stream-map
               (cons proc (map stream-cdr argstreams))))))

;= Answer ===============================================================================

(define x (stream-map show (stream-enumerate-interval 0 10)))

(print)
(print (stream-ref x 5))
(print (stream-ref x 7))





;= Test =================================================================================
