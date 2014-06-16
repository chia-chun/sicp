(define (cont-frac n d k)
  (define (calc x)
    (if (= x k)
        (/ (n x) (d x))
        (/ (n x) (+ (d x) (calc (+ x 1))))))
  (calc 1))

(define (cont-frac-iter n d k)
  (define (iter count result)
    (let ((x result))
       (if (= count 1)
           result
           (iter (- count 1) (/ (n (- count 1)) (+ (d (- count 1)) x))))))
  (iter k (/ (n k) (d k))))