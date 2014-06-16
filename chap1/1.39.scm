(define (cont-frac n d k)
  (define (iter count result)
    (let ((x result))
       (if (= count 1)
           result
           (iter (- count 1) (/ (n (- count 1)) (+ (d (- count 1)) x))))))
  (iter k (/ (n k) (d k))))

(define (tan-cf x k)
  (define (n i)
    (if (= i 1)
        x
        (- (* x x))))
  (define (d i)
    (- (* i 2) 1.0))
  (cont-frac n d k))