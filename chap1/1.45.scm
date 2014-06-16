(define tolerance 0.000000000000001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (average-damp f)
  (lambda (x) (/ (+ x (f x)) 2)))

(define (repeated f n)
  (if (= n 1)
      (lambda (x) (f x))
      (lambda (x) (f ((repeated f (- n 1)) x)))))

(define (log2 x)
  (/ (log x) (log 2)))

(define (power x n);x to the nth power
  (if (= n 1)
      x
      (* x (power x (- n 1)))))

(define (root x n);find the nth root of x
  (let ((times (floor (log2 n))))
    (fixed-point ((repeated average-damp times)
                  (lambda (y) (/ x (power y (- n 1)))))
                 1.0)))
