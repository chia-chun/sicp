(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

(define (identity x) x)

(define (inc x) (+ x 1))

(define (factorial x)
  (product identity 1 inc x))

(define (pi x)
  (define (term-1 a)
     (if (= (remainder a 2) 0)
         (+ a 2)
         (+ a 1)))
  (define (term-2 b)
     (if (= (remainder b 2) 0)
         (+ b 1)
         (+ b 2)))
  (exact->inexact (/ (* 4 (product-iter term-1 1 inc x))
                     (product-iter term-2 1 inc x))))
; using the recursive product would cause easy stack overflow
