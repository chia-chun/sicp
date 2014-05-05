(define (smallest-divisor n)
  (find-divisor n 2))

(define (next n)
  (if (= n 2) 3
      (+ n 2)))

(define (find-divisor n test-divisor)
  (cond ((> (* test-divisor test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (square x)
  (* x x))

(define (inc x)
  (+ x 1))

(define (identity x)
  x)

(define (gcd x y)
  (if (= (remainder y x) 0)
      x
      (gcd (remainder y x) x)))

(define (filtered-accumulate combiner filter? null-value term a next b)
  (define (iter x result)
    (if (> x b)
        result
        (iter (next x) (if (filter? x)
                           (combiner (term x) result)
                           result))))
  (iter a null-value))

(define (sum-sq-prime a b);sum of the squares of the prime numbers in a~b
  (filtered-accumulate + prime? 0 square a inc b))

(define (product-relative-prime n)
        ;product of all the positive intergers less than n, relative prime to n
  (define (relative-prime? x)
     (= (gcd x n) 1))
  (filtered-accumulate * relative-prime? 1 identity 1 inc (- n 1)))
