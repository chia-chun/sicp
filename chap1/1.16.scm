(define (fast-expt b n)
  (fast-expt-iter b n 1))

(define (fast-expt-iter b n answer)
  (cond ((= n 0) answer)
        ((= (remainder n 2) 1) (fast-expt-iter b (- n 1) (* answer b)))
        (else (fast-expt-iter (* b b) (/ n 2) answer))))