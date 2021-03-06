(load "put-get.scm")
(load "types.scm")

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else
         ((get 'deriv (operator exp))
          (operands exp)
          var))))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

;; a. This approach used the data-directed approach. The "type-tag" is the + or
;;    * symbol. The predicates number? and variable? do not have type-tags
;;    naturally. If we want to include those two predicates, we have to identify
;;    their types manually.

;; b.

(define (sum-deriv items var)
  (let ((exp (cons '+ items)))
    (make-sum (deriv (addend exp) var)
              (deriv (augend exp) var))))

(put 'deriv '+ sum-deriv)

(define (product-deriv items var)
  (let ((exp (cons '* items)))
    (make-sum
     (make-product (multiplier exp)
                   (deriv (multiplicand exp) var))
     (make-product (deriv (multiplier exp) var)
                   (multiplicand exp)))))

(put 'deriv '* product-deriv)

;; c.

(define (exp-deriv items var)
  (let ((exp (cons '** items)))
    (make-product (exponent exp)
                  (make-product
                   (make-exponentiation (base exp)
                                        (- (exponent exp) 1))
                   (deriv (base exp) var)))))

(put 'deriv '** exp-deriv)

;; (define (deriv exp var)
;;   (cond ((number? exp) 0)
;;         ((variable? exp)
;;          (if (same-variable? exp var) 1 0))
;;         ((sum? exp)
;;          (make-sum (deriv (addend exp) var)
;;                    (deriv (augend exp) var)))
;;         ((product? exp)
;;          (make-sum
;;           (make-product (multiplier exp)
;;                         (deriv (multiplicand exp) var))
;;           (make-product (deriv (multiplier exp) var)
;;                         (multiplicand exp))))
;;         ((exponentiation? exp)
;;          (make-product (exponent exp)
;;                        (make-product
;;                         (make-exponentiation (base exp)
;;                                              (- (exponent exp) 1))
;;                         (deriv (base exp) var))))
;;         (else (error "unknown expression type - DERIV" exp))))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 . a2)
  (define (simplify-sum items symbols numbers)
    (cond ((null? items)
           (if (eq? symbols '(+))
               numbers
               (if (= numbers 0)
                   (if (null? (cddr symbols))
                       (cadr symbols)
                       symbols)
                   (append symbols (list numbers)))))
          ((number? (car items))
           (simplify-sum (cdr items) symbols (+ numbers (car items))))
          ((sum? (car items))
           (simplify-sum (cdr items)
                         (append symbols (cdar items))
                         numbers))
          (else
           (simplify-sum (cdr items)
                         (append symbols (list (car items)))
                         numbers))))
  (if (null? a2)
      a1
      (simplify-sum (cons a1 a2) '(+) 0)))


(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-product m1 . m2)
  (define (simplify-product items symbols numbers)
    (cond ((null? items)
           (if (eq? symbols '(*))
               numbers
               (cond ((= numbers 0) 0)
                     ((= numbers 1)
                      (if (null? (cddr symbols))
                          (cadr symbols)
                          symbols))
                     (else (append symbols (list numbers))))))
          ((number? (car items))
           (simplify-product (cdr items) symbols (* numbers (car items))))
          ((product? (car items))
           (simplify-product (cdr items)
                             (append symbols (cdar items))
                             numbers))
          (else
           (simplify-product (cdr items)
                             (append symbols (list (car items)))
                             numbers))))
  (simplify-product (cons m1 m2) '(*) 1))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s)
  (if (null? (cdddr s))
      (caddr s)
      (cons '+ (cddr s))))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p)
  (if (null? (cdddr p))
      (caddr p)
      (cons '*  (cddr p))))

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(define (base e) (cadr e))

(define (exponent e) (caddr e))

(define (make-exponentiation base exponent)
  (cond ((or (=number? base 0) (=number? exponent 0)) 0)
        ((=number? exponent 1) base)
        (else (list '** base exponent))))
