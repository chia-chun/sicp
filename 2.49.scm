(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
      (lambda (segment)
        (draw-line
          ((frame-coord-map frame) (start-segment segment))
          ((frame-coord-map frame) (end-segment segment))))
      segment-list)))

(define outline-painter
  (let ((bottom-left (make-vect 0 0))
        (bottom-right (make-vect 1 0))
        (top-left (make-vect 0 1))
        (top-right (make-vect 1 1)))
    (segments->painter
      (list
        (make-segment bottom-left bottom-right)
        (make-segment bottom-right top-right)
        (make-segment top-right top-left)
        (make-segment top-left bottom-left)))))

(define x-painter
  (let ((bottom-left (make-vect 0 0))
        (bottom-right (make-vect 1 0))
        (top-left (make-vect 0 1))
        (top-right (make-vect 1 1)))
    (segments->painter
      (list
        (make-segment bottom-left top-right)
        (make-segment bottom-right top-left)))))

(define (diamond-painter frame)
  (let ((left (make-vect 0 0.5))
        (right (make-vect 1 0.5))
        (top (make-vect 0.5 1))
        (bottom (make-vect 0.5 0)))
    (segments->painter
      (list
        (make-segment left top)
        (make-segment top right)
        (make-segment right bottom)
        (make-segment bottom left)))))
