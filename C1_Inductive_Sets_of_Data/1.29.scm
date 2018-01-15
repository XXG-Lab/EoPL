(define sort-sub
  (lambda (loi)
    (if (null? loi)
        '()
        (if (null? (cdr loi))
            (cons (car loi) '())
            (if (< (car loi) (cadr loi))
                (cons (car loi) (sort-sub (cdr loi)))
                (cons (cadr loi) (sort-sub (cons (car loi) (cddr loi)))))))))

(define sort_
  (lambda (loi)
    (if (null? loi)
        '()
        (let ((sorted (sort-sub loi)))
          (cons (car sorted) (sort_ (cdr sorted)))))))

(equal? (sort_ '(8 2 5 2 3)) '(2 2 3 5 8))
