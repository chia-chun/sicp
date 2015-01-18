(load "generic-system.scm")

(define (install-=zero?-package)
  (put '=zero? '(scheme-number)
       (lambda (x) (= x 0)))
  (put '=zero? '(rational)
       (lambda (x) (= (car x) 0)))
  (put '=zero? '(complex)
       (lambda (x) (= (magnitude x) 0)))
  'done)

(install-=zero?-package)

(define (=zero? x)
  (apply-generic '=zero? x))

(define (equ? x y)
  (=zero? (sub x y)))
