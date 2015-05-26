(load "generic-system.scm")

(define coercion-table (make-table))

(define get-coercion (coercion-table 'lookup-proc))
(define put-coercion (coercion-table 'insert-proc!))

(define (apply-generic op . args)
  (define (coerce-or-same a b)
    ;; tests that if a = b or a can be coarced to b
    (or (eqv? a b) (get-coercion a b)))

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
                              (append (get-coercion (car old) target) new))))
      (coerce-type-iter types '())))

  (let ((type-tags (map type-tag args)))
    (if (get-type type-tags)
        (let ((new-type-tags (coerce-type type-tags)))
          (apply (get op new-type-tags) (map contents args)))
        (error "No method for these types"
               (list op type-tags)))))

;; If there is a method to coerce a to b, and another to coerce b to c, but not
;; directly a to c, this function cannot use those methods to coerce a to c.
