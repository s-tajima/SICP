(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (make-segment start end)
  (cons start end))


(define (start-segment segment)
  (car segment))


(define (end-segment segment)
  (cdr segment))


(define (make-point x y)
  (cons x y))


(define (x-point point)
  (car point))


(define (y-point point)
  (cdr point))

; ====================================

(define (perimeter width height)
  (+ (* width 2) (* height 2)))

(define (area width height)
  (* width height))

; ====================================

(define (make-rectangle start end)
  (cons start end)) 

(define (width-rectangle rectangle)
  (abs (- (x-point (start-segment rectangle))
          (x-point (end-segment rectangle)))))

(define (height-rectangle rectangle)
  (abs (- (y-point (start-segment rectangle))
          (y-point (end-segment rectangle)))))

(define rectangle (make-rectangle (make-point 1 2) (make-point 2 4)))

(print (perimeter (width-rectangle rectangle) (height-rectangle rectangle)))
(print (area      (width-rectangle rectangle) (height-rectangle rectangle)))

; ====================================

(define (make-rectangle2 width height)
  (cons width height)) 

(define (width-rectangle2 rectangle)
  (car rectangle))
  
(define (height-rectangle2 rectangle)
  (cdr rectangle))

(define rectangle2 (make-rectangle2 1 2))

(print (perimeter (width-rectangle2 rectangle2) (height-rectangle2 rectangle2)))
(print (area      (width-rectangle2 rectangle2) (height-rectangle2 rectangle2)))




