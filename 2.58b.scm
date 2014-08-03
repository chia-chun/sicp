(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ((exponentiation? exp)
         (make-product (exponent exp)
                       (make-product
                        (make-exponentiation (base exp)
                                             (- (exponent exp) 1))
                        (deriv (base exp) var))))
        (else (error "unknown expression type - DERIV" exp))))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 . a2)
  (define (simplify-sum items symbols numbers)
    (cond ((null? items)
           (if (null? symbols)
               numbers
               (if (= numbers 0)
                   symbols
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
      (insert '+ (simplify-sum (cons a1 a2) '() 0))))

(define (insert symbol items)
  (cond ((not (pair? items)) items)
        ((null? (cdr items))
         (list (car items)))
        (else (append (list (car items) symbol) (insert symbol (cdr items))))))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-product m1 . m2)
  (define (simplify-product items symbols numbers)
    (cond ((null? items)
           (if (null? symbols)
               numbers
               (cond ((= numbers 0) 0)
                     ((= numbers 1) symbols)
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
  (if (null? m2)
      m1
      (insert '* (simplify-product (cons m1 m2) '() 1))))

(define (sum? x)
  (cond ((null? x) #f)
        ((not (pair? x)) #f)
        ((eq? (car x) '+) #t)
        (else (sum? (cdr x)))))

(define (addend s) (car s))

(define (augend s) (caddr s))

(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))

(define (multiplier p) (car p))

(define (multiplicand p) (caddr p))

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(define (base e) (cadr e))

(define (exponent e) (caddr e))

(define (make-exponentiation base exponent)
  (cond ((or (=number? base 0) (=number? exponent 0)) 0)
        ((=number? exponent 1) base)
        (else (list '** base exponent))))
