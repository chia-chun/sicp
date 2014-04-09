(define (fun1 n) ;recursive process
  (cond ((= n 0) 0)
        ((= n 1) 1)
        ((= n 2) 2)
        (else (+ (fun1 (- n 1)) (* 2 (fun1 (- n 2))) (* 3 (fun1 (- n 3)))))))

(define (fun2 n) ;iterative process
  (fun2-iter 2 1 0 n))

(define (fun2-iter a b c count)
  (if (= count 0)
      c
      (fun2-iter (+ a (* 2 b) (* 3 c)) a b (- count 1))))
