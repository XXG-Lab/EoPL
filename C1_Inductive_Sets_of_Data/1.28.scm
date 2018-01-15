(define merge
  (lambda (loi1 loi2)
    (if (null? loi1)
        (if (null? loi2)
            '()
            loi2)
        (if (null? loi2)
            loi1
            (if (<= (car loi1) (car loi2))
                (cons (car loi1) (merge (cdr loi1) loi2))
                (cons (car loi2) (merge loi1 (cdr loi2))))))))

(equal? (merge '(1 4) '(1 2 8)) '(1 1 2 4 8))
(equal? (merge '(35 62 81 90 91) '(3 83 85 90)) '(3 35 62 81 83 85 90 90 91))
