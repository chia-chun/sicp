(define (double f)
  (lambda (x) (f (f x))))

(define (inc x)
  (+ x 1))

(define (square x)
  (* x x))

(((double (double double)) inc) 5);=21
