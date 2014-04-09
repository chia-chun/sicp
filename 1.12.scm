(define (element x y) ;the xth element of the yth row
  (cond ((= y 1) 1)
        ((= x 1) 1)
        ((= x y) 1)
        (else (+ (element (- x 1) (- y 1)) (element x (- y 1))))))
