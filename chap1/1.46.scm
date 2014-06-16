(define (iterative-improve good-enough? improve)
  (lambda (guess)
          (if (good-enough? guess)
              guess
              ((iterative-improve good-enough? improve) (improve guess)))))

(define (fixed-point f)
  (define (fp-good-enough? guess)
    (< (abs (- (f guess) guess)) 0.0000001))
  (define (fp-improve guess)
    (/ (+ guess (f guess)) 2))
  ((iterative-improve fp-good-enough? fp-improve) 1.0))

(define (sqrt x)
  (define (sqrt-good-enough? guess)
     (< (/ (abs (- (* guess guess) x)) x) 0.0000001))
  (define (sqrt-improve guess)
     (/ (+ guess (/ x guess)) 2))
  ((iterative-improve sqrt-good-enough? sqrt-improve) 1.0))
