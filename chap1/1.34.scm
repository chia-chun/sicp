(define (f g)
  (g 2))

(define (square x)
  (* x x))

(f square)

(f (lambda (z) (* z (+ z 1))))

;scheme@(guile-user)> (f f)
;ERROR: In procedure 2:
;ERROR: Wrong type to apply: 2

;Entering a new prompt.  Type `,bt' for a backtrace or `,q' to continue.
;scheme@(guile-user) [1]> ,bt
;           0 (2 2)
