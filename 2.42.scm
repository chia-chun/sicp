;; This program provides a solution as a list of row number.
;; For example, the solution provided in Fig. 2.8 is (3 7 2 8 5 1 4 6).
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
          (lambda (positions) (safe? k positions))
          (flatmap
            (lambda (rest-of-queens)
              (map (lambda (new-row)
                     (adjoin-position new-row
                                      k
                                      rest-of-queens))
                   (enumerate-interval 1 board-size)))
            (queen-cols (- k 1))))))
  (queen-cols board-size))

(define (safe? k positions)
  (define (get kth positions)
    (if (= kth 1)
        (car positions)
        (get (- kth 1) (cdr positions))))
  (define (row-safe? k positions)
    (if (= k 1)
        #t
        (and (not (= (get k positions) (car positions)))
             (row-safe? (- k 1) (cdr positions)))))
  (define (diagonal-safe? k positions)
    (if (= k 1)
        #t
        (and (not (= (- k 1) (- (get k positions) (car positions))))
             (not (= (- (car positions) (get k positions)) (- k 1)))
             (diagonal-safe? (- k 1) (cdr positions)))))
  (and (row-safe? k positions) (diagonal-safe? k positions)))

(define (adjoin-position new-row k rest-of-queens)
  (append rest-of-queens (list new-row)))

(define empty-board '())

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
