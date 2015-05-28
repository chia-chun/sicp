(load "generic-system.scm")

(define coercion-table (make-table))

(define get-coercion (coercion-table 'lookup-proc))
(define put-coercion (coercion-table 'insert-proc!))

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

  (define (coerce-type types)
    (let ((target (get-type types)))
      (define (coerce-type-iter old new)
        (if (null? old)
            new
            (coerce-type-iter (cdr old)
                              (append (list (coerce-or-same (car old) target))
                                      new))))
      (coerce-type-iter types '())))

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

;; If there is a method to coerce a to b, and another to coerce b to c, but not
;; directly a to c, this function cannot use those methods to coerce a to c.

(define (install-coercion-package)
  (define (scheme-number->complex n)
    (make-complex-from-real-imag (contents n) 0))
  (define (scheme-number->rational n)
    (make-rational (contents n) 1))
  (put-coercion 'scheme-number 'rational scheme-number->rational)
  (put-coercion 'scheme-number 'complex scheme-number->complex)
  'done)

(install-coercion-package)
