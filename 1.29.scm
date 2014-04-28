(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (cube x)
  (* x x x))

(define (intergral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b) dx))

(define (s-intergral f a b n);Integral computed using Simpson's Rule
  (define (next-2 x)
     (+ x (/ (- b a) n 0.5)))
  (/ (*
      (- b a)
      (+ (f a)
         (f b)
         (* 4 (sum f (+ a (/ (- b a) n)) next-2 (- b (/ (- b a) n))))
         (* 2 (sum f (+ a (/ (- b a) n 0.5)) next-2 (- b (/ (- b a) n 0.5))))))
     3
     n))
