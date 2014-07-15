(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (car (cdr branch)))

(define (total-weight mobile)
  (define (weight-of-branch branch)
    (if ((not (pair? (branch-structure branch))))
        (branch-structure branch)
        (total-weight (branch-structure branch))))
  (+ (weight-of-branch (left-branch mobile))
     (weight-of-branch (right-branch mobile))))
