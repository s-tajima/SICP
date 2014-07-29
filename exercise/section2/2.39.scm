;= Question =============================================================================
;
; 問題 2.39
;
; reverse(問題2.18)の, 問題2.38のfold-rightとfold-left を使った, 次の定義を完成せよ:
;
; (define (reverse sequence)
;   (fold-right (lambda (x y) ⟨??⟩) nil sequence))
;
; (define (reverse sequence)
;   (fold-left (lambda (x y) ⟨??⟩) nil sequence))
;
;
;= Prepared =============================================================================

(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (fold-right op initial (cdr sequence)))))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

;= Answer ===============================================================================

(define (r-reverse sequence)
  (fold-right (lambda (x y) (append y (list x))) () sequence))

(define (l-reverse sequence)
  (fold-left (lambda (x y) (cons y x)) () sequence))

(print (r-reverse (list 1 2 3 4)))
(print (l-reverse (list 1 2 3 4)))

