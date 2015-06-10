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

;; The raise function contains only the label of the original type.
;; My method is to create a new raise-table to contain the labels of both the
;; original type and the raised type.

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

;; We have to manually enter a bottom number (scheme-number 1 in this case).

(create-raise-table (make-scheme-number 1))

;; We can still use the apply-generic function defined in 2.82, only with a new
;; ger-coercion function.

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

(define (apply-generic-new op . args)
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
                                 (append (list (transform (car old)))
                                         new))))
      (coerce-content-iter args '())))

  (let ((type-tags (map type-tag args)))
    (if (get-type type-tags)
        (let ((new-content (coerce-content args)))
          (apply (get op (map type-tag new-content)) (map contents new-content)))
        (error "No method for these types"
               (list op type-tags)))))
;; Example:
;; scheme@(guile-user)> (apply-generic-new 'add
;;                                         (make-rational 3 2)
;;                                         (make-complex-from-real-imag 2 3))
;; $1 = (complex rectangular 7/2 . 3)
