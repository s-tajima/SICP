(print (let* ((x 3)
              (y (+ x 2))
              (z (+ x y 5)))
             (* x z)))


(print 
  (let ((x 3))
    (let ((y (+ x 2)))
      (let ((z (+ x y 5)))
        (* x z)))))

