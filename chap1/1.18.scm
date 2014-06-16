(define (* a b)
  (*-iter a b 0))

(define (*-iter a b answer)
  (cond ((= b 0) answer)
        ((= (remainder b 2) 1) (*-iter a (- b 1) (+ answer a)))
        (else (*-iter (double a) (halve b) answer))))

(define (double x)
  (+ x x))

(define (halve x)
  (/ x 2))