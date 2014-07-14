;= Question =============================================================================
;
; 問題 2.35
; 
; 2.2.2節のcount-leavesをアキュムレーションとして再定義せよ.
; 
; (define (count-leaves t)
;   (accumulate ⟨??⟩ ⟨??⟩ (map ⟨??⟩ ⟨??⟩)))
; 
;= Prepared =============================================================================

(define x (cons (list 1 2) (list 3 4)))
(define (count-leaves x)
  (cond ((null? x) 0)  
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))


(define x (cons (list 1 2) (list 3 4)))
(define y (cons (list 1 2) (list 3 (list 4 5 6))))
(define z (cons (list 1 2) (list 3 (list (list 4 41 42) 5 6))))
(define a ())
;= Answer ===============================================================================

(define (count-leaves2 t)
  (accumulate (lambda (init next) (+ init next)) 
              0 
              (map (lambda (t) (if (pair? t) (count-leaves2 t) 1)) t)))

(print (count-leaves  x))
(print (count-leaves2 x))

(print (count-leaves  y))
(print (count-leaves2 y))

(print (count-leaves  z))
(print (count-leaves2 z))

(print (count-leaves  a))
(print (count-leaves2 a))
