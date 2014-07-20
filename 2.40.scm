(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (unique-pairs n)
  (accumulate append
              '()
              (map (lambda (i)
                     (map (lambda (j) (list i j))
                          (enumerate-interval 1 (- i 1))))
                   (enumerate-interval 1 n))))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (prime? n)
  (fast-prime? n 100))
;; The following prime test is from exercise 1.28
(define (square x)
  (* x x))

(define (expmod-signal base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (if (and (not (= (expmod-signal base (/ exp 2) 1)))
                  (not (= (expmod-signal base (/ exp 2) (- m 1))))
                  (= 1 (remainder (square (expmod-signal base (/ exp 2) m))
                                  m)))
             0
             (remainder (square (expmod-signal base (/ exp 2) m)) m)))
        (else
         (remainder (* base (expmod-signal base (- exp 1) m))
                    m))))

(define (MR-test n) ;Miller-Rabin test
  (define (try-it a)
     (= (expmod-signal a (- n 1) n) 1))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ((MR-test n) (fast-prime? n (- times 1)))
        (else #f)))

(define (runtime)
  (+ (* 1000000 (car (gettimeofday))) (cdr (gettimeofday))))

(define (timed-prime-test n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (fast-prime? n 100)
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
