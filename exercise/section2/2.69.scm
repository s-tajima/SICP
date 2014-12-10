;= Question =============================================================================
;
; 問題 2.69
;
; 次の手続きは引数として記号と頻度の対のリストをとり, 
; (どの記号も一つの対以外には現れない.) Huffmanアルゴリズムに従い, 
; Huffman符号化木を生成する.
;
; (define (generate-huffman-tree pairs)
;   (successive-merge (make-leaf-set pairs)))
;
; make-leaf-setは上にある手続きで, 対のリストを葉の順序づけられた集合へ変換する. 
; successive-mergeは自分で書く手続きで, make-code-treeを使い, 
;
; 集合の最小重みの要素を順に合体させ, 要素が一つになったら止める. 
; それが目的のHuffman木である. 
; (この手続きは多少ややこしいが, 複雑ではない. 
; 複雑な手続きを設計していると思ったら, 確実にどこか違っている. 
; 順序づけられた集合の表現を使っていることを活用しなければならない.)
; 
;= Prepared =============================================================================

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
           (adjoin-set (make-leaf (car pair)    ; 記号
                                  (cadr pair))  ; 頻度
                       (make-leaf-set (cdr pairs))))))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

;= Answer ===============================================================================

(define (successive-merge leaf-set)
  (if (= 1 (length leaf-set))
      (car leaf-set)
      (successive-merge (adjoin-set (make-code-tree (car leaf-set) (cadr leaf-set)) (cddr leaf-set) ))))

(print (generate-huffman-tree (list (list 'A 4)  (list 'B 2)  (list 'C 1)  (list 'D 1))))

