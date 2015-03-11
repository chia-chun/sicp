(load "generic-system.scm")

(define coercion-table (make-table))

(define get-coercion (coercion-table 'lookup-proc))
(define put-coercion (coercion-table 'insert-proc!))

(define (apply-generic op . args)
  (define (test-coercion type types)
    ;; tests that if the items in the list "types" can be coarced to "type"
    (if (null? (cdr types))
        (get-coercion (car types) type)
        (if (get-coercion (car types) type)
            (test-coercion type (cdr types))
            #f)))
  (define (test-coercion-get-type types)
    ;; get the type to which all items in the list "types" can be coareced
    (if (test-coercion (car types) (cdr types))
        (car types)
        (test-coercion (cadr types) (append (car types) (cddr types))))

  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (let ((type1 (car type-tags))
                (type2 (cadr type-tags))
                (a1 (car args))
                (a2 (cadr args)))
            (let ((t1->t2 (get-coercion type1 type2))
                  (t2->t1 (get-coercion type2 type1)))
              (cond (t1->t2
                     (apply-generic op (t1->t2 a1) a2))
                    (t2->t1
                     (apply-generic op a1 (t2->t1 a2)))
                    (else
                     (error "No method for these types"
                            (list op type-tags)))))))
