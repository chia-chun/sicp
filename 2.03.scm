(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

;1st representation
(define (make-rectangle bottom-left top-right)
  (cons bottom-left top-right))

(define (width rectangle)
  (- (car (cdr rectangle)) (car (car rectangle))))

(define (height rectangle)
  (- (cdr (cdr rectangle)) (cdr (car rectangle))))

;2nd representation
(define (make-rectangle top-left bottom-right)
  (cons top-left bottom-right))

(define (width rectangle)
  (- (car (cdr rectangle)) (car (car rectangle))))

(define (height rectangle)
  (- (cdr (car rectangle)) (cdr (cdr rectangle))))

;Calculating the area and the perimeter
(define (area rectangle)
  (* (width rectangle) (height rectangle)))

(define (perimeter rectangle)
  (* 2 (+ (width rectangle) (height rectangle))))
