;= Question =============================================================================
;
; 問題 3.22
; 
; キューを一対のポインタで表現する代りに, 
; キューを局所状態を持つ手続きとして作ることが出来る. 
; 
; 局所状態は通常のリストの最初と最後へのポインタからなる. 
; 従ってmake-queueは次の形である.
; 
; (define (make-queue)
;   (let ((front-ptr ... )
;         (rear-ptr ... ))
;        ⟨内部手続きの定義⟩
;        (define (dispatch m) ...)
;        dispatch))
; 
; make-queueの定義を完成し, この表現を使ってキューを実装せよ.
;
;= Prepared =============================================================================
 

;= Answer ===============================================================================

(define (make-queue)
  (let ((front-ptr ()) 
        (rear-ptr ()))

  (define (empty-queue?) (null? front-ptr))

  (define (set-front-ptr! item) (set! front-ptr item))

  (define (set-rear-ptr! item) (set! rear-ptr item))

  (define (insert-queue! item)
    (let ((new-pair (cons item '())))
         (cond ((empty-queue?)
                (set-front-ptr! new-pair)
                (set-rear-ptr! new-pair)
                new-pair)
               (else
                 (set-cdr! rear-ptr new-pair)
                 (set-rear-ptr! new-pair)
                 new-pair)))) 

  (define (delete-queue!)
    (cond ((empty-queue?)
           (error "DELETE! called with an empty queue"))
          (else
            (set-front-ptr! (cdr front-ptr))
            ))) 

  (define (print-queue)
    (print front-ptr))
  
  (define (dispatch m)
    (cond ((eq? m 'insert-queue!) insert-queue!)
          ((eq? m 'delete-queue!) delete-queue!)
          ((eq? m 'print-queue) print-queue)
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       m))))
  dispatch))

;= Test =================================================================================

(define q1 (make-queue))
((q1 'insert-queue!) '1)
((q1 'print-queue))
((q1 'insert-queue!) '2)
((q1 'print-queue))
((q1 'insert-queue!) '3)
((q1 'print-queue))
((q1 'delete-queue!))
((q1 'print-queue))
