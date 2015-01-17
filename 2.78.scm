(load "generic-system.scm")

(define (attach-tag tag datum)
  (if (eqv? tag 'scheme-number)
      datum
      (cons tag datum)))

(define (type-tag datum)
  (cond ((pair? datum)
         (car datum))
        ((number? datum)
         'scheme-number)
        (else (error "Bad typed datum -- TYPE-TAG" datum))))

(define (contents datum)
  (cond ((pair? datum)
         (cdr datum))
        ((number? datum)
         datum)
        (else (error "Bad typed datum -- CONTENTS" datum))))
