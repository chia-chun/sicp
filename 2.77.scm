(load "generic-system.scm")

(define z '(complex rectangular 3 . 4))

(magnitude z)

(apply-generic 'magnitude z)

(apply-generic 'magnitude '(complex rectangular 3 . 4))

(let ((type-tags (map type-tag (list z))))
    (let ((proc (get 'magnitude type-tags)))
      (if proc
          (apply proc (map contents (list z)))
          (error "No method for the given types -- APPLY-GENERIC"
                 (list 'magnitude type-tags)))))
;; Of note, there is still subtle difference between (list z) and '(z).
;;
;; scheme@(guile-user)> (map type-tag '(z))
;; ERROR: In procedure scm-error:
;; ERROR: Bad typed datum -- TYPE-TAG z
;; scheme@(guile-user)> (map type-tag (list z))
;; (complex)

(apply (get 'magnitude '(complex)) '((rectangular 3 . 4)))

(magnitude '(rectangular 3 . 4))

(apply-generic 'magnitude '(rectangular 3 . 4))

(apply (get 'magnitude '(rectangular)) '((3 . 4)))

;; 5
