;= Question =============================================================================
; 
; 問題 2.68
; 
; encode手続きは引数として通信文と木をとり, 符号化された通信文のビットのリストを作る.
; 
; (define (encode message tree)
;   (if (null? message)
;       '()
;       (append (encode-symbol (car message) tree)
;               (encode (cdr message) tree))))
; 
; encode-symbolは自分で書く手続きで, 
; 与えられた木に従って与えられた記号を符号化したビットのリストを返すものである.
; encode-symbolの設計では, 記号が木になければ, エラーとしなければならない. 
; 出来た手続きを問題2.67で得た結果と, 例題の木を使って符号化し,
; 元の例題の通信文と同じかどうかを見てテストせよ.
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

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                    (make-leaf 'B 2)
                    (make-code-tree (make-leaf 'D 1)
                                    (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))


;= Answer ===============================================================================

(define (encode-symbol symbol tree)
  (cond ((leaf? tree) ())
        ((member symbol (symbols (left-branch tree))) (cons 0 (encode-symbol symbol (left-branch tree))))
        ((member symbol (symbols (right-branch tree))) (cons 1 (encode-symbol symbol (right-branch tree))))
        (else (error "bad symbol -- " symbol))))


(print (encode '(A D A B B C A) sample-tree))

