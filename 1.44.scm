(define (smooth f)
  (lambda (x)
    (/ (+ (f (+ x dx))
          (f x)
          (f (- x dx)))
       3)))

(define dx 0.00001)

(define (repeated f n)
  (if (= n 1)
      (lambda (x) (f x))
      (lambda (x) (f ((repeated f (- n 1)) x)))))

(define (n-smooth f n);n-fold smoother function
  ((repeated smooth n) f))
