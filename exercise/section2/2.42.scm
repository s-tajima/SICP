;= Question =============================================================================
;
; 「エイトクィーンパズル」はチェス盤上に八つのクィーンを, 
; どのクィーンも他のと当りにならないような
; (つまりどの二つのクィーンも同じ行, 同じ列, 同じ斜めの筋にないような)置き方を問う.
; 一つの解を図2.8に示す. 
; パズルを解く一つの方法は, 一つのクィーンを各列に置きながら, 盤上を進む. 
; k-1 個のクィーンを置いたら, 
; k個目のクィーンをすでに盤上に置いたどのクィーンとも当らない場所に置かなければならない. 
; このやり方を再帰的に形式化出来る. 
; 盤の最初のk-1列にk-1個のクィーンを置くすべての方法の列が生成してあるとしよう. 
; この各方法に対し, k番目の列の各行にクィーンを置き, 場所を拡大した集合を生成する. 
; これをフィルタに通し, k番目のクィーンが, 他のクィーンに対して安全な場所だけを残す. 
; これで最初のk列にk個のクィーンを置くすべての方法の並びが出来る. 
; この方法を継続すると, 唯一の解ではなく, パズルの全解が得られる.
; この解の手続きをqueensとして実装する. 
; これはn × nのチェス盤上にnクィーンを置く問題のすべての解の並びを返す. 
; queensの内部手続きqueen-colsは盤の最初のk列にクィーンを置くすべての方法の並びを返す.
; 
;    (define (queens board-size)
;      (define (queen-cols k)  
;        (if (= k 0)
;            (list empty-board)
;            (filter
;              (lambda (positions) (safe? k positions))
;              (flatmap
;                (lambda (rest-of-queens)
;                        (map (lambda (new-row)
;                                     (adjoin-position new-row k rest-of-queens))
;                             (enumerate-interval 1 board-size)))
;                (queen-cols (- k 1))))))
;      (queen-cols board-size))
;
; この手続きで, rest-of-queensは最初のk-1列にk-1個のクィーンを置く方法であり, 
; new-rowはk列目にクィーンの置ける行の案である. 
; 盤上の位置の集合の表現を実装し,
; 位置の集合に新しい場所の座標を連結する手続きadjoin-position, 
; 場所の空集合を表現するempty-boardと合せてプログラムを完成せよ. 
; また, 他のクィーンに対し, 
; k番目のクィーンが安全な場所を決める手続きsafe?を書かなければならない. 
; (他のクィーンは互いに安全であることが保証されているので, 
; 新しいクィーンが安全なことだけ調べればよい.) 
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

;= Answer ===============================================================================

(define (queens board-size)
  (define (queen-cols k)  
    (if (= k 0)
        (list empty-board)
        (filter
          (lambda (positions) (safe? k positions))
          (flatmap
            (lambda (rest-of-queens)
                    (map (lambda (new-row)
                                 (adjoin-position new-row k rest-of-queens))
                         (enumerate-interval 1 board-size)))
            (queen-cols (- k 1))))))
      (queen-cols board-size))

(define (empty-board) nil)

(define (adjoin-position new-row k rest-of-queens) 
  (list rest-of-queens (list new-row k)))

(define (safe? k positions) #t)

(print (adjoin-position 4 3 (list (list 1 2) (list 2 4) (list 3 1))))

(print (queens 4))


