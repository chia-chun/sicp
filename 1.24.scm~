(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (* test-divisor test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= ( remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (runtime)
  (+ (* 1000000 (car (gettimeofday))) (cdr (gettimeofday))))

(define (timed-prime-test n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime n (- (runtime) start-time))))

(define (report-prime n  elapsed-time)
  (newline)
  (display n)
  (display "***")
  (display elapsed-time))

(define (search-for-primes a b)
  (cond ((< b a) (newline) (display "End") (newline))
        ((= b a) (timed-prime-test a))
        ((= (remainder a 2) 0) (search-for-primes (+ 1 a) b))
        (else (timed-prime-test a)
              (search-for-primes (+ 2 a) b))))
