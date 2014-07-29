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

(define diamond-painter
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

(define wave
  (segments->painter
   (list
    (make-segment
     (make-vect 0.006 0.840)
     (make-vect 0.155 0.591))
    (make-segment
     (make-vect 0.006 0.635)
     (make-vect 0.155 0.392))
    (make-segment
     (make-vect 0.304 0.646)
     (make-vect 0.155 0.591))
    (make-segment
     (make-vect 0.298 0.491)
     (make-vect 0.155 0.392))
    (make-segment
     (make-vect 0.304 0.646)
     (make-vect 0.403 0.646))
    (make-segment
     (make-vect 0.298 0.491)
     (make-vect 0.354 0.492))
    (make-segment
     (make-vect 0.403 0.646)
     (make-vect 0.348 0.845))
    (make-segment
     (make-vect 0.354 0.492)
     (make-vect 0.249 0.000))
    (make-segment
     (make-vect 0.403 0.000)
     (make-vect 0.502 0.293))
    (make-segment
     (make-vect 0.502 0.293)
     (make-vect 0.602 0.000))
    (make-segment
     (make-vect 0.348 0.845)
     (make-vect 0.403 0.999))
    (make-segment
     (make-vect 0.602 0.999)
     (make-vect 0.652 0.845))
    (make-segment
     (make-vect 0.652 0.845)
     (make-vect 0.602 0.646))
    (make-segment
     (make-vect 0.602 0.646)
     (make-vect 0.751 0.646))
    (make-segment
     (make-vect 0.751 0.646)
     (make-vect 0.999 0.343))
    (make-segment
     (make-vect 0.751 0.000)
     (make-vect 0.637 0.491))
    (make-segment
     (make-vect 0.637 0.491)
     (make-vect 0.999 0.144)))))
