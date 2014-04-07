(define (>= x y)
  (or (> x y) (= x y)))

(define (sum-of-squares x y)
  (+ (* x x) (* y y)))

(define (fun x y z);sum of squares of the 2 larger numbers
  (cond ((and (>= x y) (>= z y)) (sum-of-squares x z))
        ((and (>= y x) (>= z x)) (sum-of-squares y z))
        ((and (>= y z) (>= x z)) (sum-of-squares y x))))
