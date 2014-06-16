(define (add-rat x y)
  (make-rat (+ (* (car x) (cdr y))
               (* (car y) (cdr x)))
            (* (cdr x) (cdr y))))

(define (sub-rat x y)
  (make-rat (- (* (car x) (cdr y))
               (* (car y) (cdr x)))
            (* (cdr x) (cdr y))))

(define (mul-rat x y)
  (make-rat (* (car x) (car y))
            (* (cdr x) (cdr y))))

(define (div-rat x y)
  (make-rat (* (car x) (cdr y))
            (* (cdr x) (car y))))

(define (equal-rat? x y)
  (= (* (car x) (cdr y))
     (* (car y) (cdr x))))

(define (gcd x y)
  (if (= (remainder y x) 0)
      x
      (gcd (remainder y x) x)))

(define (make-rat n d)
  (let ((g (abs (gcd n d))))
    (if (< d 0)
        (cons (- (/ n g)) (- (/ d g)))
        (cons (/ n g) (/ d g)))))

(define (print-rat x)
  (display (car x))
  (display "/")
  (display (cdr x))
  (newline))
