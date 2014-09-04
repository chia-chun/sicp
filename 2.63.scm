(define (make-tree entry left-branch right-branch)
  (list entry left-branch right-branch))

(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))
; O(nlogn)

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree) (copy-to-list (right-branch tree)
                                                       result-list)))))
  (copy-to-list tree '()))

; O(n)

(define tree-1 '(7 (3 (1 () ()) (5 () ())) (9 () (11 () ()))))
; (tree->list-1 tree-1)
; (tree->list-2 tree-1)
; (1 3 5 7 9 11)

(define tree-2 '(3 (1 () ()) (7 (5 () ()) (9 () (11 () ())))))

; (tree->list-1 tree-2)
; (tree->list-2 tree-2)
; (1 3 5 7 9 11)

(define tree-3 '(5 (3 (1 () ()) ()) (9 (7 () ()) (11 () ()))))

; (tree->list-1 tree-3)
; (tree->list-2 tree-3)
; (1 3 5 7 9 11)
