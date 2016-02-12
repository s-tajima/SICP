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

(use srfi-27)

(define (random-numbers-in-range low high init)
  (define random-max 12344)
  (define random-numbers
    (cons-stream init
                 (stream-map rand-update random-numbers)))
  (define (rand-update x)
    (let ((a (expt 2 32))
          (c 1103515245)
          (m 12345))
         (modulo (+ (* a x) c) m)))
  (let ((range (- high low)))
       (stream-map (lambda (x) 
                           (+ low (* range (/ x random-max))))
                   random-numbers)))

(define (stream-map proc . argstreams)
  (if (null? (car argstreams))
      the-empty-stream
      (cons-stream
        (apply proc (map stream-car argstreams))
        (apply stream-map
               (cons proc (map stream-cdr argstreams))))))

(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (cons-stream
      (/ passed (+ passed failed))
      (monte-carlo
        (stream-cdr experiment-stream) passed failed)))
  (if (stream-car experiment-stream)
      (next (+ passed 1) failed)
      (next passed (+ failed 1))))

(define (estimate-integral P x1 x2 y1 y2)
  (define ex-stream
    (stream-map (lambda (x y) (P x y))
                (random-numbers-in-range x1 x2 788) 
                (random-numbers-in-range y1 y2 2310)))
  (let ((area (* (- x2 x1) (- y2 y1))))
       (stream-map (lambda (frac)
                           (* frac area))
                   (monte-carlo ex-stream 0 0))))

(define pi-stream
  (estimate-integral (lambda (x y)
                             (< (+ (square x) (square y)) 1.0))
                     -1.0 1.0 -1.0 1.0))

(stream-head pi-stream 10000)


;= Answer ===============================================================================


;= Test =================================================================================
