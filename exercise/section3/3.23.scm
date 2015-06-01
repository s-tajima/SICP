;= Question =============================================================================
;
; 問題 3.23
; 
; デキュー(deque)(「douebl-ended queue」)は,
; 項目が先頭と後尾のどちらででも挿入や削除が出来る並びである.
; デキューの演算は, 構成子 make-queue, 述語empty-queue?, 選択子front-queueと
; rear-queue, 変更子front-insert-queue!, rear-insert-queue!, front-delete-queue!
; および rear-delete-queue!である.
; 対を使ってデキューを表現する方法を示し,
; 演算を実装せよ.23 すべての演算はΘ(1)ステップで達成しなければならない.
;
;= Prepared =============================================================================
 

;= Answer ===============================================================================

(define (front-ptr queue) (car queue))

(define (rear-ptr queue) (cdr queue))

(define (set-front-ptr! queue item) (set-car! queue item))

(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (empty-queue? queue) (null? (front-ptr queue)))

(define (make-queue) (cons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

(define (rear-queue queue)
  (if (empty-queue? queue)
      (error "REAR called with an empty queue" queue)
      (car (rear-ptr queue))))


(define (front-delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
          (set-front-ptr! queue (cdr (front-ptr queue)))
          queue))) 

; 新規作成
(define (rear-delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
          (set-cdr! (rear-ptr queue) '())
          (set-rear-ptr! queue (cdr (rear-ptr queue)))
          queue))) 


; 新規作成
(define (front-insert-queue! queue item)
  (let ((new-pair (cons item '())))
       (cond ((empty-queue? queue)
              (set-front-ptr! queue new-pair)
              (set-rear-ptr! queue new-pair)
              queue)
             (else
               (set-cdr! new-pair (front-ptr queue) )
               (set-front-ptr! queue new-pair)
               queue)))) 

(define (rear-insert-queue! queue item)
  (let ((new-pair (cons item '())))
       (cond ((empty-queue? queue)
              (set-front-ptr! queue new-pair)
              (set-rear-ptr! queue new-pair)
              queue)
             (else
               (set-cdr! (rear-ptr queue) new-pair)
               (set-rear-ptr! queue new-pair)
               queue)))) 

(define (print-queue q1)
  (print (front-ptr q1)))

;= Test =================================================================================

(define q1 (make-queue))
(print-queue (rear-insert-queue! q1 'a))
(print-queue (rear-insert-queue! q1 'b))
(print-queue (rear-insert-queue! q1 'c))
(print-queue (front-delete-queue! q1))
(print-queue (front-delete-queue! q1))
(print-queue (front-delete-queue! q1))

(print-queue (rear-insert-queue! q1 'a))
(print-queue (rear-insert-queue! q1 'b))
(print-queue (front-insert-queue! q1 'c))
(print-queue (front-insert-queue! q1 'd))
(print-queue (rear-insert-queue! q1 'e))
(print-queue (rear-insert-queue! q1 'f))
(print-queue (front-delete-queue! q1))
(print-queue (rear-delete-queue! q1))



