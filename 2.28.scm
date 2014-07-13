(define (fringe items)
  (cond ((not (pair? items)) items)
        ((null? (cdr items)) (fringe (car items)))
        (else (append (list (car items)) (fringe (car (cdr items)))))))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(define (fringe2 items)
  (let ((result 1))
    (define (fringe-iter li)
      (cond ((not (pair? li)) (define result (cons li result)))
            (else (fringe-iter (car li)) (fringe-iter (cdr li))))
    (fringe-iter items))))
