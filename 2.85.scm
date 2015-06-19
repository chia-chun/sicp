(load "generic-system.scm")

(define (install-real-package)
  (define (tag x)
    (attach-tag 'real x))
  (put 'add '(real real)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(real real)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(real real)
       (lambda (x y) (tag (* x y))))
  (put 'div '(real real)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'real
       (lambda (x) (tag x)))
  'done)

(install-real-package)

(define (make-real n)
  ((get 'make 'real) n))

(define (install-raise)
  (define (raise-number integer)
    (make-rational integer 1))

  (define (raise-rational rational)
    (make-real (/ (car rational) (cdr rational))))

  (define (raise-real real)
    (make-complex-from-real-imag real 0))

  (put 'raise '(scheme-number) raise-number)
  (put 'raise '(rational) raise-rational)
  (put 'raise '(real) raise-real)
  'done)

(install-raise)

(define (raise x) (apply-generic 'raise x))

(define (install-project)
  (define (project-complex complex)
    (make-real (real-part complex)))

  (define (project-real real)
    (make-rational (round (numerator real))
                   (round (denominator real))))

  (define (project-rational rational)
    (make-scheme-number (round (/ (car rational) (cdr rational)))))

  (put 'project '(rational) project-rational)
  (put 'project '(real) project-real)
  (put 'project '(complex) project-complex)
  'done)

(install-project)

(define (project x) (apply-generic 'project x))

(define (install-=zero?-package)
  (put '=zero? '(scheme-number)
       (lambda (x) (= x 0)))
  (put '=zero? '(rational)
       (lambda (x) (= (car x) 0)))
  (put '=zero? '(real)
       (lambda (x) (= x 0)))
  (put '=zero? '(complex)
       (lambda (x) (= (magnitude x) 0)))
  'done)

(install-=zero?-package)

(define (=zero? x)
  (apply-generic '=zero? x))

(define (install-equ?-package)
  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y) (= x y)))
  (put 'equ? '(rational rational)
       (lambda (x y) (= (/ (car x) (cdr x)) (/ (car y) (cdr y)))))
  (put 'equ? '(real real)
       (lambda (x y) (= x y)))
  (put 'equ? '(complex complex)
       (lambda (x y) (and (= (real-part x) (real-part y))
                          (= (imag-part x) (imag-part y)))))
  'done)

(install-equ?-package)

(define (equ? x y)
  (apply-generic 'equ? x y))

(define (drop x)
  (if (or (not (pair? x))
          (eqv? (type-tag x) 'scheme-number))
      x
      (if (equ? x (raise (project x)))
          (drop (project x))
          x)))

(define raise-table (make-table))

(define get-raise (raise-table 'lookup-proc))
(define put-raise (raise-table 'insert-proc!))

(define (create-raise-table arg)
  (cond ((get 'raise (list (type-tag arg)))
         (put-raise 'raise
                    (type-tag arg)
                    (type-tag ((get 'raise (list (type-tag arg)))
                               (contents arg))))
         (create-raise-table ((get 'raise (list (type-tag arg)))
                              (contents arg))))))

(create-raise-table (make-scheme-number 1))

(define (get-coercion a b)
  ;; The new get-coercion is based on raise.
  ;; The output is to repeat raise for n times.
  (define (repeat-raise n)
    (if (= n 0)
        (lambda (x) x)
        (lambda (x) ((repeat-raise (- n 1)) (raise x)))))

  (define (get-coercion-iter a b m)
    (cond ((eqv? a b) (repeat-raise m))
          ((get-raise 'raise a)
           (get-coercion-iter (get-raise 'raise a) b (+ m 1)))
          (else #f)))

  (get-coercion-iter a b 0))

(define (apply-generic op . args)
  (define (coerce-or-same a b)
    ;; tests that if a = b or a can be coarced to b
    (if (eqv? a b)
        a
        (get-coercion a b)))

  (define (test-coercion types type)
    ;; tests that if the items in the list "types" can be coarced to "type"
    (if (null? (cdr types))
        (coerce-or-same (car types) type)
        (if (coerce-or-same (car types) type)
            (test-coercion (cdr types) type)
            #f)))

  (define (get-type types)
    ;; get the type to which all items in the list "types" can be coarced
    (define (get-type-iter types items)
      (if (null? items)
          #f
          (if (test-coercion types (car items))
              (car items)
              (get-type-iter types (cdr items)))))
    (let ((items types))
      (get-type-iter types items)))

  (define (coerce-content args)
    (let ((target (get-type (map type-tag args))))
      (define (transform arg)
        (if (eqv? (type-tag arg) target)
            arg
            ((get-coercion (type-tag arg) target) arg)))
      (define (coerce-content-iter old new)
        (if (null? old)
            new
            (coerce-content-iter (cdr old)
                                 (append new
                                         (list (transform (car old)))))))
      (coerce-content-iter args '())))

  (let ((type-tags (map type-tag args)))
    (if (get-type type-tags)
        (let ((new-content (coerce-content args)))
          (if (memv op '(raise project =zero? equ? real-part imag-part))
              ;; We must exclude operations whose answer should not be "dropped".
              (apply (get op (map type-tag new-content))
                     (map contents new-content))
              (drop (apply (get op (map type-tag new-content))
                           (map contents new-content)))))
        (error "No method for the given types -- APPLY-GENERIC"
               (list op type-tags)))))
