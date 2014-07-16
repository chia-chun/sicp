(define (make-mobile left right)
  (cons left right))

(define (make-branch length structure)
  (cons length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cdr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cdr branch))

(define (total-weight mobile)
  (define (weight-of-branch branch)
    (if (not (pair? (branch-structure branch)))
        (branch-structure branch)
        (total-weight (branch-structure branch))))
  (+ (weight-of-branch (left-branch mobile))
     (weight-of-branch (right-branch mobile))))

(define (balanced? mobile)
  (define (torque branch)
    (if (not (pair? (branch-structure branch)))
        (* (branch-length branch) (branch-structure branch))
        (* (branch-length branch) (total-weight (branch-structure branch)))))
  (if (not (pair? mobile))
      #t
      (and (balanced? (branch-structure (left-branch mobile)))
           (balanced? (branch-structure (right-branch mobile)))
           (= (torque (left-branch mobile)) (torque (right-branch mobile))))))
