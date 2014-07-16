(define (tree-map1 proc tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (proc tree))
        (else (cons (tree-map1 (car tree)) (tree-map1 (cdr tree))))))

(define (tree-map2 proc tree)
  (map (lambda (sub-tree) (if (pair? sub-tree)
                              (tree-map2 proc sub-tree)
                              (proc sub-tree)))
       tree))

(define (square-tree1 tree)
  (tree-map1 square tree))

(define (square-tree2 tree)
  (tree-map2 square tree))

(define (square x)
  (* x x))
