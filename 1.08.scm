(define (cube x)
  (* x x x))

(define (cubert-iter guess x)
  (if (good-enough? guess x)
          guess
          (cubert-iter (improve guess x)
                     x)))

(define (improve guess x)
  (/ (+ (/ x (* guess guess)) (* guess 2)) 3))

(define (good-enough? guess x)
  (< (abs (/ (- guess (improve guess x)) guess)) 0.001))

(define (abs x)
  (if (< x 0) (- x) x))

(define (cubert x)
  (cubert-iter 1.0 x))
