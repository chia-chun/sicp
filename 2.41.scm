(define (find-sum n s)
  (filter (lambda (triple) (triple-sum-s? triple s)) (unique-triples n)))

(define (unique-triples n)
  (accumulate append
              '()
              (map (lambda (k)
                     (accumulate append
                                 '()
                                 (map (lambda (j)
                                        (map (lambda (i) (list i j k))
                                             (enumerate-interval 1 (- j 1))))
                                      (enumerate-interval 1 (- k 1)))))
                   (enumerate-interval 1 n))))

(define (triple-sum-s? triple s)
  (= s (+ (car triple) (cadr triple) (caddr triple))))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
