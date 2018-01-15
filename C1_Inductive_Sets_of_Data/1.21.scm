(define product-rev
  (lambda (sos1 sos2 o-sos2)
    (if (null? sos1)
        '()
        (if (null? sos2)
            (product-rev (cdr sos1) o-sos2 o-sos2)
            (cons (cons (car sos1) (cons (car sos2) '()))
                  (product-rev sos1 (cdr sos2) o-sos2))))))

(define product
  (lambda (sos1 sos2)
    (product-rev sos1 sos2 sos2)))

(equal? (product '(a b c) '(x y)) '((a x) (a y) (b x) (b y) (c x) (c y)))
