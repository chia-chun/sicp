(define (* a b)
  (cond ((= b 0) 0)
        ((= b 1) a)
        ((= (remainder b 2) 0) (* (double a) (halve b)))
        (else (+ a (* a (- b 1))))))

(define (double x)
  (+ x x))

(define (halve x)
  (/ x 2))