(define (make-interval a b) (cons a b))

(define (lower-bound interval) (min (car interval) (cdr interval)))

(define (upper-bound interval) (max (car interval) (cdr interval)))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

(define (mul-interval x y)
  (cond ((and (<  (lower-bound x) 0)
              (<  (upper-bound x) 0)
              (<  (lower-bound y) 0)
              (<  (upper-bound y) 0))
         (make-interval (* (upper-bound x) (upper-bound y))
                        (* (lower-bound x) (lower-bound y))))
        ((and (<  (lower-bound x) 0)
              (<  (upper-bound x) 0)
              (<  (lower-bound y) 0)
              (>= (lower-bound y) 0))
         (make-interval (* (lower-bound x) (upper-bound y))
                        (* (lower-bound x) (lower-bound y))))
        ((and (<  (lower-bound x) 0)
              (<  (upper-bound x) 0)
              (>= (lower-bound y) 0)
              (>= (lower-bound y) 0))
         (make-interval (* (lower-bound x) (upper-bound y))
                        (* (upper-bound x) (lower-bound y))))
        ((and (<  (lower-bound x) 0)
              (>= (upper-bound x) 0)
              (<  (lower-bound y) 0)
              (<  (lower-bound y) 0))
         (make-interval (* (upper-bound x) (lower-bound y))
                        (* (lower-bound x) (lower-bound y))))
        ((and (<  (lower-bound x) 0)
              (>= (upper-bound x) 0)
              (<  (lower-bound y) 0)
              (>= (lower-bound y) 0))
         (make-interval (min (* (lower-bound x) (upper-bound y))
                             (* (upper-bound x) (lower-bound y)))
                        (max (* (lower-bound x) (lower-bound y))
                             (* (upper-bound x) (upper-bound y)))))
        ((and (<  (lower-bound x) 0)
              (>= (upper-bound x) 0)
              (>= (lower-bound y) 0)
              (>= (lower-bound y) 0))
         (make-interval (* (lower-bound x) (upper-bound y))
                        (* (upper-bound x) (upper-bound y))))
        ((and (>= (lower-bound x) 0)
              (>= (upper-bound x) 0)
              (<  (lower-bound y) 0)
              (<  (lower-bound y) 0))
         (make-interval (* (upper-bound x) (lower-bound y))
                        (* (lower-bound x) (upper-bound y))))
        ((and (>= (lower-bound x) 0)
              (>= (upper-bound x) 0)
              (<  (lower-bound y) 0)
              (>= (lower-bound y) 0))
         (make-interval (* (upper-bound x) (lower-bound y))
                        (* (upper-bound x) (upper-bound y))))
        ((and (>= (lower-bound x) 0)
              (>= (upper-bound x) 0)
              (>=  (lower-bound y) 0)
              (>= (lower-bound y) 0))
         (make-interval (* (lower-bound x) (lower-bound y))
                        (* (upper-bound x) (upper-bound y))))))

(define (>= x y)
  (or (> x y) (= x y)))

(define (div-interval x y)
  (if (or (< (* (lower-bound y) (upper-bound y)) 0)
          (= (* (lower-bound y) (upper-bound y)) 0))
      ((lambda () (display "error")
                  (newline)))
      (mul-interval x (make-interval (/ 1.0 (upper-bound y))
                                     (/ 1.0 (lower-bound y))))))
