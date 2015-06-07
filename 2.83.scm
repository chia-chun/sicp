(load "generic-system.scm")

(define (install-raise)
  (define (raise-number integer)
    (make-rational integer 1))

  (define (raise-rational rational)
    (attach-tag 'real (/ (car rational) (cdr rational))))

  (define (raise-real real)
    (make-complex-from-real-imag real 0))

  (put 'raise '(scheme-number) raise-number)
  (put 'raise '(rational) raise-rational)
  (put 'raise '(real) raise-real)
  'done)

(install-raise)

(define (raise x) (apply-generic 'raise x))
