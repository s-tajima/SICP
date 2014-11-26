;= Question =============================================================================
; 
; 問題 2.65
;
; 問題2.63と2.64の結果を使い, 
; (釣合った)二進木として実装されている集合の
; union-setとintersection-setを&Theta(n)で実装せよ.41
;
;= Prepared =============================================================================

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
           (let ((left-result (partial-tree elts left-size)))
                (let ((left-tree (car left-result))
                      (non-left-elts (cdr left-result))
                      (right-size (- n (+ left-size 1))))
                     (let ((this-entry (car non-left-elts))
                           (right-result (partial-tree (cdr non-left-elts)
                                                       right-size)))
                          (let ((right-tree (car right-result))
                                (remaining-elts (cdr right-result)))
                               (cons (make-tree this-entry left-tree right-tree)
                                     remaining-elts))))))))

(define (union-set-list set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((= (car set1) (car set2)) (union-set-list (cdr set1) set2))
        ((< (car set1) (car set2)) (cons (car set1) (union-set-list (cdr set1) set2)))
        (else                      (cons (car set2) (union-set-list (cdr set2) set1)))))

;= Answer ===============================================================================

(define tree1 (list->tree (list 1 3 5 7 9)))
(define tree2 (list->tree (list 2 4 6 8 10)))

(define (union-set tree1 tree2)
  (list->tree (union-set-list (tree->list-1 tree1) (tree->list-1 tree2))))

(print (union-set tree1 tree2))


