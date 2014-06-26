(define (cons a b)
  (* (2-power a) (3-power b)))

(define (2-power a)
  (if (= a 0)
      1
      (* 2 (2-power (- a 1)))))

(define (3-power b)
  (if (= b 0)
      1
      (* 3 (3-power (- b 1)))))

(define (car c)
  (get-2-power c))

(define (cdr c)
  (get-3-power c))

(define (get-2-power c)
  (define (get-2-power-iter c a)
    (if (= (remainder c 2) 0)
        (get-2-power-iter (/ c 2) (+ a 1))
        a))
  (get-2-power-iter c 0))

(define (get-3-power c)
  (define (get-3-power-iter c b)
    (if (= (remainder c 3) 0)
        (get-3-power-iter (/ c 3) (+ b 1))
        b))
  (get-3-power-iter c 0))
