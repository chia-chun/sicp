(define (square-list-iter1 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items '()))
; (square-list-iter1 (list 1 2 3 4))
; (16 9 4 1)

(define (square-list-iter2 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items '()))
; (square-list-iter2 (list 1 2 3 4))
; ((((() . 1) . 4) . 9) . 16)
; We cannot use cons to contruct a list from the end.


(define (square-list-iter3 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (append answer
                      (list (square (car things)))))))
  (iter items '()))
; (square-list-iter3 (list 1 2 3 4))
; (1 4 9 16)
; We have to use append to contrust the answer list.

(define (square x)
  (* x x))
