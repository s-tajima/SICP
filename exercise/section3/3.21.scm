;= Question =============================================================================
;
; 問題 3.21
; 
; Ben Bitdiddleは上に述べたキューの実装をテストしようと決めた. 
; Lispの解釈系に手続きを入力し, 次々と試みた.
; 
; (define q1 (make-queue))
; 
; (insert-queue! q1 'a)
; ((a) a)
; 
; (insert-queue! q1 'b)
; ((a b) b)
; 
; (delete-queue! q1)
; ((b) b)
; 
; (delete-queue! q1)
; (() b)
; 
; 「全部違っている」と彼は文句をいう. 
; 「解釈系の応答を見ると, 最後の項目がキューに二度挿入されている. 
; 二つの項目を削除しても, 二番目のb は残っていて, キューは空の筈なのに空でない.」 
; Eva Lu AtorはBenは事情が分っていないという. 
; 「項目がキューに二回入っているのではないわ.」彼女は説明する. 
; 「標準のLispの印字プログラムは, キューの表現をどうすればよいか知らないだけなの. 
; キューを正しく印字して見たいなら,キューのための印字手続きを自分で定義しなければならない.」 
; Eva Luのいっていることを説明せよ. 
; 特にBenの例がどうしてあのような印字結果を生じたか説明せよ. 
; 入力としてキューをとり, キューの中の項目の並びを印字する手続きprint-queueを定義せよ. 
;
;= Prepared =============================================================================
 
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

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
       (cond ((empty-queue? queue)
              (set-front-ptr! queue new-pair)
              (set-rear-ptr! queue new-pair)
              queue)
             (else
               (set-cdr! (rear-ptr queue) new-pair)
               (set-rear-ptr! queue new-pair)
               queue)))) 

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
          (set-front-ptr! queue (cdr (front-ptr queue)))
          queue))) 

;= Answer ===============================================================================

(define (print-queue q1)
  (print (front-ptr q1)))

(define q1 (make-queue))
(print-queue (insert-queue! q1 'a))
(print-queue (insert-queue! q1 'b))
(print-queue (delete-queue! q1))
(print-queue (delete-queue! q1))



;= Test =================================================================================

