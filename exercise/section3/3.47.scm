;= Question =============================================================================
;  
; 問題 3.47
; 
; (大きさnの)セマフォは相互排除器の一般化である. 
; 相互排除器と同様に, セマフォは獲得と解放の操作を提供するが, 最大n個のプロセスまで, 
; それを並列的に獲得出来るという点で, より一般的である. 
; そのセマフォを獲得しようとするそれ以上のプロセスは解放操作を待たなければならない. 
; セマフォを
; 
; a. 相互排除器を使って
; 
; b. test-and-set!を使って
; 
; 実装せよ. 
;
;= Prepared =============================================================================

(define (make-semaphore)
  (let ((cell (list 0)))
       (define (the-semaphore m)
         (cond ((eq? m 'acquire)
                (if (test-and-set! cell)
                    (the-semaphore 'acquire))) ; retry
               ((eq? m 'release) (set! cell (- cell 1)))
               ((eq? m 'current) cell)))
       the-semaphore))

(define (test-and-set! cell)
  (if (< 2  (car cell))
      #t
      (begin (set-car! cell (+ (car cell) 1))
             #f)))

;= Answer ===============================================================================

(define semaphore (make-semaphore))

(print (semaphore 'current))
(print (semaphore 'acquire))

(print (semaphore 'current))
(print (semaphore 'acquire))

(print (semaphore 'current))
(print (semaphore 'acquire))

(print (semaphore 'current))
(print (semaphore 'acquire))

;= Test =================================================================================


