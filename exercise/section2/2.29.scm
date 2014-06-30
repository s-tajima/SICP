;= Question =============================================================================
;
; 問題 2.29
;
; 二進モービルは, 左の枝と右の枝の二つの枝で出来ている. 
; それぞれの枝はある長さの棒で, そこから錘か, 別の二進モービルがぶら下っている.
; 二進モービルを二つの枝から(例えばlistを使って)出来ている合成データで表現出来る:
; 
; (define (make-mobile left right)
;   (list left right))
; 
; 一つの枝はlength(数でなければならない)と, 
; (単なる錘を表現する)数か別のモービルかであるstructureで構成する:
; 
; (define (make-branch length structure)
;   (list length structure))
; 
;  a. これに対応する選択子(モービルの枝を返す)left-branchと right-branchと,
;     (枝の部品を返す)branch-lengthと branch-structureを書け.
;
;  b. この選択子を使い, モービルの全重量を返す手続きtotal-weightを定義せよ.
;
;  c. モービルは最上段左の枝による回転力と, 最上段右の枝による回転力が等しく, 
;     (つまり左の棒の長さと棒の左にかかっている重さを掛けたものが右側の対応する積に等しく,) 
;     しかも枝にぶら下っている部分モービルのそれぞれが釣合っている時, 
;     釣合っている(balanced)という.
;     二進モービルが釣合っているかどうかをテストする述語を設計せよ.
;
;  d. 構成子が
; 
;     (define (make-mobile left right)
;       (cons left right))
;
;     (define (make-branch length structure)
;       (cons length structure))
;
;     となるようにモービルの表現を変更したとする. 
;     新しい表現に対応するにはプログラムをどのくらい変更しなければならないか.
; 
;= Prepared =============================================================================
(define (make-mobile left right)
  (list left right))
 
(define (make-branch length structure)
  (list length structure))

(define b1 (make-branch 1 2))
(define b2 (make-branch 3 4))
(define m1 (make-mobile b1 b2))

(define b3 (make-branch 1 m1))
(define m2 (make-mobile b3 b1))

(define b4 (make-branch 2 m2))
(define b5 (make-branch 2 5))

(define root-mobile (make-branch b4 b5)) ; W : 8 + 5 = 13

;= Answer ===============================================================================

; a
(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (car (cdr branch)))


; b
(define (total-weight mobile)
  (let ((l (branch-structure (left-branch mobile)))
        (r (branch-structure (right-branch mobile))))
       (if (pair? mobile) 
           (+ (if (pair? l) (total-weight l) (if (null? l) 0 l))
              (if (pair? r) (total-weight r) (if (null? r) 0 r)))
           mobile)))

; c
(define (branch-weight b)
  (let ((s (branch-structure b)))
    (if (not (pair? s))
        s
        (total-weight s))))

(define (balanced? m)
  (define (branch-balance b)
    (* (branch-length b) (branch-weight b)))
  (if (not (pair? m))
      #t
      (let ((l (left-branch m))
            (r (right-branch m)))
        (and (= (branch-balance l)
                (branch-balance r))
             (balanced? (branch-structure l))
             (balanced? (branch-structure r))))))

(print (balanced? (make-mobile (make-branch 2 5) (make-branch 1 10))))
(print (balanced? (make-mobile (make-branch 1 5) (make-branch 1 10))))



