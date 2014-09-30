(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge leaves)
  (if (null? cddr)
      leaves
      (successive-merge (adjoin-set (make-code-tree (car leaves)
                                                    (cadr leaves))
                                    (cddr leaves)))))


(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define(make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)   ;symbol
                               (cadr pair)) ;frequency
                    (make-leaf-set (cdr pairs))))))

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (let ((dict (tree->dict tree)))
    (define (lookup symbol dict)
      (cond ((null? dict) (error "The symbol is not in the tree"))
            ((equal? symbol (caar dict)) (cadar dict))
            (else (lookup symbol (cdr dict)))))
    (lookup symbol dict)))

(define (tree->dict tree);convert a tree to a dictionary
  (if (leaf? tree)
      (list (list (symbol-leaf tree) '()))
      (append (add-bit 0 (tree->dict (left-branch tree)))
              (add-bit 1 (tree->dict (right-branch tree))))))

(define (add-bit bit dict)
  (if (null? dict)
      '()
      (cons (list (caar dict) (append (list bit) (cadar dict)))
            (add-bit bit (cdr dict)))))

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit - CHOOSE-BRANCH" bit))))

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree (make-leaf 'B 2)
                                  (make-code-tree (make-leaf 'D 1)
                                                  (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

; (A D A B B C A)
