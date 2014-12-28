(load "put-get.scm")
(load "types.scm")

(define (get-record name file)
  ((get 'record (type-tag file) name)))

;; The division's file should be structured with a type-tag and its contents.
;; The division should also provide a procedure to get a specified record from
;; a specified file, and put that procedure in the table. The procedure should
;; return #f if the record is not in the file.

(define (get-salary name file)
  (get 'salary-record (type-tag file)) (get-record name file))

;; The division should also provide a procedure to get the salary information
;; from a specified file, and put that procedure in the table.

(define (find-employee-record name files)
  (cond ((null? files) (error "not in the files"))
        ((find-employee-record name (car files))
         (find-employee-record name (car files)))
        (else (find-employee-record name (cdr files)))))

;; When taking over a new company, Insatiable only needs to provide a procedure
;; to get a specified record from a specified file, and a procedure to get the
;; salary information from a specified file. Most of the system do not change.
