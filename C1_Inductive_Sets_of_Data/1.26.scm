(define up-rev
  (lambda (fst lst)
    (if (null? fst)
        (if (null? lst)
            '()
             (if (list? (car lst))
                 (up-rev (car lst) (cdr lst))
                 (cons (car lst) (up-rev '() (cdr lst)))))
        (cons (car fst) (up-rev (cdr fst) lst)))))

(define up
  (lambda (lst)
    (up-rev '() lst)))

(equal? (up '((1 2) (3 4))) '(1 2 3 4))
(equal? (up '((x (y)) z)) '(x (y) z))
