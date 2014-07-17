(define (count-leaves t)
  (accumulate +
              0
              (map (lambda (x) 1) (enumerate-tree t))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-tree items)
  (cond ((null? items) '())
        ((not (pair? items)) (list items))
        (else (append (enumerate-tree (car items))
                      (enumerate-tree (cdr items))))))
