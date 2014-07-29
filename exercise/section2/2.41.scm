;= Question =============================================================================
;
; 問題 2.41
; 
; 与えられた整数nに対し, 
; nより小さいか等しい相異る正の整数i, j, kの順序づけられた三つ組で, 
; 和が与えられた整数sになるものをすべて見つけよ.
;
;= Prepared =============================================================================

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      ()
      (cons low (enumerate-interval (+ low 1) high))))

;(accumulate append
;            ()
;            (map (lambda (i)
;                         (map (lambda (j) (list i j))
;                              (enumerate-interval 1 (- i 1))))
;                 (enumerate-interval 1 n)))

(define (flatmap proc seq)
  (accumulate append () (map proc seq)))


(define (permutations s)
  (if (null? s)                   ; 空集合?
      (list ())                   ; 空集合を含むリスト
      (flatmap (lambda (x)
                       (map (lambda (p) (cons x p))
                            (permutations (remove x s))))
               s)))

(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))


;= Answer ===============================================================================
(define (s-sum? trio s) 
  (= (+ (car trio) (cadr trio) (caddr trio)) s))
  

(define (make-trios-sum trio)
  (list (car trio) (cadr trio) (caddr trio) (+ (car trio) (cadr trio) (caddr trio))))

(define (unique-trios n)
  (flatmap (lambda (i)
                   (flatmap (lambda (j)
                                    (map (lambda (k) (list i j k))
                                         (enumerate-interval 1 (- j 1))))
                            (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (sum-trios n s)
  (map make-trios-sum
       (filter (lambda (trio) (s-sum? trio s))
               (unique-trios n))))

(print (sum-trios 9 10))
