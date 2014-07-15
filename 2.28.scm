(define (fringe items)
  (cond ((not (pair? items)) items)
        ((not (pair? (car items))) (cons (car items) (fringe (cdr items))))
        (else (append (fringe (car items)) (fringe (cdr items))))))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))
