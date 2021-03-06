(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
     (if (> x b)
         result
         (iter (next x) (combiner (term x) result))))
  (iter a null-value))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (product term a next b)
  (accumulate * 1 term a next b))

(define (product-iter term a next b)
  (accumulate-iter * 1 term a next b))

(define (identity x) x)

(define (inc x) (+ x 1))

(define (factorial x)
  (product identity 1 inc x))

(define (factorial-iter x)
  (product-iter identity 1 inc x))
