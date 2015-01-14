(load "generic-system.scm")

(define z '((complex) (rectangular) 3 4))

(magnitude z)

(apply-generic 'magnitude z)

(apply-generic 'magnitude '((complex) (rectangular) 3 4))

(apply (get 'magnitude '(complex)) '((rectangular) 3 4))

(magnitude '((rectangular) 3 4))

(apply-generic 'magnitude '((rectangular) 3 4))

(apply (get 'magnitude '(rectangular)) '(3 4))

;; 5
