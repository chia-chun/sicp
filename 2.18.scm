(define (reverse list)
  (if (null? list)
      list
      (append (reverse (cdr list)) (car list))))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))
