;= Question =============================================================================
;
; 問題 3.77
; 
; 上で使ったintegral手続きは, 3.5.2節の整数の無限のストリームの「暗黙の」定義に似ている.
; 別の方法として (やはり3.5.2節の) integers-starting-fromに
; 更によく似たintegralの定義を与えることが出来る:
; 
; (define (integral integrand initial-value dt)
;   (cons-stream initial-value
;                (if (stream-null? integrand)
;                    the-empty-stream
;                    (integral (stream-cdr integrand)
;                              (+ (* dt (stream-car integrand))
;                                 initial-value)
;                              dt))))
; 
; ループのあるシステムで使うと, この手続きにはintegralの初版と同じ問題がある.
; この手続きを修正し, integrandを遅延引数としてとり, 上のsolve手続きで使えるようにせよ.
;
;= Prepared =============================================================================

(load "./stream.scm")

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

(define (integral delayed-integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (let ((integrand (force delayed-integrand)))
                      (add-streams (scale-stream integrand dt)
                                   int))))
  int)


;= Answer ===============================================================================

(define (solve2 f y0 dt)
  (define y (integral2 (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

(define (integral2 delayed-integrand initial-value dt)
  (cons-stream initial-value
               (let ((integrand (force delayed-integrand)))
                    (if (stream-null? integrand)
                        the-empty-stream
                        (integral2 (delay (stream-cdr integrand))
                                   (+ (* dt (stream-car integrand))
                                      initial-value)
                                   dt)))))

;= Test =================================================================================

(stream-head (solve  (lambda (y) y) 1 0.001) 10)
(print)
(stream-head (solve2 (lambda (y) y) 1 0.001) 10)

