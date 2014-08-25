;= Question =============================================================================
;
; Louis Reasonerは問題2.42をやるのにおそろしく時間がかかった. 
; 彼の手続きqueensは動くように見えたがすごく遅かった. 
; (Louisは6 × 6を解くのさえ, 待つわけにいかなかった.) 
; LouisがEva Lu Atorに助けを求めた時, 彼女は彼がflatmapの写像の入れ子の順を入れ替えて
; 
; (flatmap
;   (lambda (new-row)
;           (map (lambda (rest-of-queens)
;                        (adjoin-position new-row k rest-of-queens))
;                (queen-cols (- k 1))))
;   (enumerate-interval 1 board-size))
; 
; と書いていると指摘した. この入替えがプログラムの実行を遅くする理由を説明せよ. 
; 問題2.42のプログラムがパズルを解く時間をTとし, 
; Louisのプログラムがエイトクィーンパズルを解くのにどのくらいかかるか推定せよ.
;
;= Prepared =============================================================================
(define (enumerate-interval low high)
  (if (> low high)
      ()
      (cons low (enumerate-interval (+ low 1) high))))

(define (flatmap proc seq)
  (accumulate append () (map proc seq)))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (queens board-size)
  (define (queen-cols k)  
    (if (= k 0)
        (list empty-board)
        (filter
          (lambda (positions) (safe? k positions))
          (flatmap
            (lambda (new-row)
                    (map (lambda (rest-of-queens)
                                 (adjoin-position new-row k rest-of-queens))
                         (queen-cols (- k 1))))
            (enumerate-interval 1 board-size)))))
      (queen-cols board-size))

(define (adjoin-position new-row k rest-of-queens)
    (append rest-of-queens (list (list new-row k))))

(define (get k l)
  (if (< k 2) (car l) (get (- k 1) (cdr l))))

(define (getUntil k l)
  (if (> k 1) (cons (car l) (getUntil (- k 1) (cdr l))) ()))

(define (getX p) (car p))
(define (getY p) (car (cdr p)))

(define (safe? k positions)
  (define (conflict? new-pos queen)
    (cond ((= (getX queen) (+ (getX new-pos) (- (getY queen) (getY new-pos)))) #t)
          ((= (getX queen) (- (getX new-pos) (- (getY queen) (getY new-pos)))) #t)
          ((= (getX new-pos) (getX queen)) #t)
          (else #f)))
  (define (hoge one others)
    (if (pair? others)
        (and (not (conflict? one (car others))) (hoge one (cdr others)))
        #t))
  (let ((newone (get k positions))
        (others (getUntil k positions)))
       (hoge newone others)))

(define empty-board ())

;= Answer ===============================================================================

(print (queens 3))


