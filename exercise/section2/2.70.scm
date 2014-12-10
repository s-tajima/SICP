;= Question =============================================================================
;
; 問題 2.70
; 
; 次の八記号のアルファベットと相対頻度は, 
; 1950年代のロックの歌の叙情詩を効率よく符号化するよう設計された. 
; (「アルファベット」の「記号」は必ずしも個々の文字ではないことに注意)
; 
; A     2          NA   16
; BOOM  1          SHA   3
; GET   2          YIP   9
; JOB   2          WAH   1
; 
; (問題2.69の)generate-huffman-treeを使って対応するHuffman木を生成し, 
; (問題2.68の)encodeを使って次の通信文を符号化せよ:
; 
; Get a job
; Sha na na na na na na na na
; Get a job
; Sha na na na na na na na na
; Wah yip yip yip yip yip yip yip yip yip
; Sha boom
; 
; 符号化に何ビット必要か. 八記号アルファベットの固定長符号を使うとこの歌を符号化するのに必要な最小ビット数はいくらか.
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

(define (successive-merge leaf-set)
  (if (= 1 (length leaf-set))
      (car leaf-set)
      (successive-merge (adjoin-set (make-code-tree (car leaf-set) (cadr leaf-set)) (cddr leaf-set) ))))

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (cond ((leaf? tree) ())
        ((member symbol (symbols (left-branch tree))) (cons 0 (encode-symbol symbol (left-branch tree))))
        ((member symbol (symbols (right-branch tree))) (cons 1 (encode-symbol symbol (right-branch tree))))
        (else (error "bad symbol -- " symbol))))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
                (choose-branch (car bits) current-branch)))
             (if (leaf? next-branch)
                 (cons (symbol-leaf next-branch)
                       (decode-1 (cdr bits) tree))
                 (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

;= Answer ===============================================================================

(define pairs
  (list 
      (list 'A     2) (list 'NA   16) 
      (list 'BOOM  1) (list 'SHA   3)
      (list 'GET   2) (list 'YIP   9)
      (list 'JOB   2) (list 'WAH   1)))
 
(define tree (generate-huffman-tree pairs))

(define text '(GET A JOB SHA NA NA NA NA NA NA NA NA GET A JOB SHA NA NA NA NA NA NA NA NA WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP SHA BOOM))
(print (encode text tree))
(print (decode (encode text tree) tree))








