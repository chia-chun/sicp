(define x (list 1 2 3))

(define y (list 4 5 6))

(append x y)
;(1 2 3 4 5 6)

(cons x y)
;((1 2 3) 4 5 6)

(list x y)
;((1 2 3) (4 5 6))

;; scheme@(guile-user)> (define z (list x y))
;; scheme@(guile-user)> (car z)
;; (1 2 3)
;; scheme@(guile-user)> (cdr z)
;; ((4 5 6))
;; scheme@(guile-user)> (car (car z))
;; 1
;; scheme@(guile-user)> (car (cdr z))
;; (4 5 6)
