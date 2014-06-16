(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (cube x)
  (* x x x))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b) dx))

(define (s-integral f a b n);Integral computed using Simpson's Rule
  (define h (/ (- b a) n))
  (define (next-2 x)
     (+ x (* 2 h)))
  (* (/ h 3)
     (+ (f a)
        (f b)
        (* 4 (sum f (+ a h) next-2 (- b h)))
        (* 2 (sum f (+ a (* 2 h)) next-2 (- b (* 2 h)))))))
