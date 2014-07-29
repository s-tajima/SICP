;= Question =============================================================================
;
; 問題 2.40
;
; 与えられた整数nに対し, 1 ≤ j < i ≤ nの対(i, j)の並びを生成する手続き 
; unique-pairsを定義せよ. 
; unique-pairsを使って, 上のprime-sum-pairsの定義を簡単にせよ.
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

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))


(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap
                 (lambda (i)
                         (map (lambda (j) (list i j))
                              (enumerate-interval 1 (- i 1))))
                 (enumerate-interval 1 n)))))

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

(define (prime? x)
  (define (sub-prime? x y)
    (cond ((= x y) #t)
          ((= (remainder x y) 0) #f)
          (else (sub-prime? x (+ y 1)))))
  (sub-prime? x 2))

;= Answer ===============================================================================

(print (accumulate append
                   ()
                   (map (lambda (i)
                                (map (lambda (j) (list i j))
                                     (enumerate-interval 1 (- i 1))))
                        (enumerate-interval 1 3))))

(define (unique-pairs n) 
  (accumulate append
              ()
              (map (lambda (i)
                           (map (lambda (j) (list i j))
                                (enumerate-interval 1 (- i 1))))
                   (enumerate-interval 1 n))))


(define (my-prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (unique-pairs n))))

(print (my-prime-sum-pairs 6))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (unique-pairs n))))

(print (prime-sum-pairs 6))
