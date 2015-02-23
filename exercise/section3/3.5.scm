;= Question =============================================================================
;
; 問題3.4
; 
; 問題3.3のmake-accountを, もう一つの局所状態変数を加えるように修正し, 
; 口座が不正なパスワードで連続七回アクセスされると, 
; 手続き call-the-copsを起動するようにせよ.
; 
;= Prepared =============================================================================

(use srfi-27)
(random-source-randomize! default-random-source)
(define rand (lambda () (random-integer 1000000000)))
(define (square x) (* x x))

(define (rand-update x)
  (let ((a (expt 2 32))
        (c 1103515245)
        (m 12345))
       (modulo (+ (* a x) c) m)))


(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
            (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

;= Answer ===============================================================================

(define (random n) (* n (random-real)))

(define (random-in-range low high)
  (let ((range (- high low)))
      (+ low (random range))))

(define (p)
  (>= 1 (+ (square (- (random-in-range 0 1) 1)) (square (- (random-in-range 0 1) 1)))))

(define (estimate-integral trials)
  (* 4.0 (monte-carlo trials p)))

(print (estimate-integral 1000000))
