(require eopl)

(define list-sum
  (lambda (lst)
    (list-sum/k lst 0)))

(define list-sum/k
  (lambda (lst cont)
    (if (null? lst)
        cont
        (list-sum/k (cdr lst) (+ (car lst) cont)))))

(equal? (list-sum '(1 2 3)) 6)
