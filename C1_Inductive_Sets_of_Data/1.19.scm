(define list-set
  (lambda (lst n x)
    (if (null? lst)
        '()
        (if (zero? n)
            (cons x (cdr lst))
            (cons (car lst) (list-set (cdr lst) (- n 1) x))))))

(equal? (list-set '(a b c d) 2 '(1 2)) '(a b (1 2) d))
(equal? (list-set '(a b c d) 3 '(1 5 10)) '(a b c (1 5 10)))
