(define sort-sub
  (lambda (pred loi)
    (if (null? loi)
        '()
        (if (null? (cdr loi))
            (cons (car loi) '())
            (if (pred (car loi) (cadr loi))
                (cons (car loi) (sort-sub pred (cdr loi)))
                (cons (cadr loi) (sort-sub pred (cons (car loi) (cddr loi)))))))))

(define sort/predicate
  (lambda (pred loi)
    (if (null? loi)
        '()
        (let ((sorted (sort-sub pred loi)))
          (cons (car sorted) (sort/predicate pred (cdr sorted)))))))

(equal? (sort/predicate < '(8 2 5 2 3)) '(2 2 3 5 8))
(equal? (sort/predicate > '(8 2 5 2 3)) '(8 5 3 2 2))
