(define flatten-rev
  (lambda (slist ret)
    (if (null? slist)
        ret
        (if (list? slist)
            (flatten-rev (cdr slist) (flatten-rev (car slist) ret))
            (cons slist ret)))))

(define flatten
  (lambda (slist)
    (reverse (flatten-rev slist '()))))

(equal? (flatten '(a b c)) '(a b c))
(equal? (flatten '((a) () (b ()) () (c))) '(a b c))
(equal? (flatten '((a b) c (((d)) e))) '(a b c d e))
(equal? (flatten '(a b (() (c)))) '(a b c))
