(define (raise-number integer)
  (make-rational (contents integer) 1)))

(define (raise-rational rational)
  (attach-tag 'real (/ (car (contents rational)) (cdr (contents rational)))))

(define (raise-real real)
  (make-complex-from-real-imag (contents real) 0))
