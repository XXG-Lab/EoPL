(define remove-eq
  (lambda (e lst)
    (if (null? lst)
        '()
        (if (eqv? e (car lst))
            (remove-eq e (cdr lst))
            (cons (car lst) (remove-eq e (cdr lst)))))))

(equal? (remove-eq 'a '(a b c)) '(b c))
(equal? (remove-eq 'b '(a b c)) '(a c))
(equal? (remove-eq 'a '(a a b)) '(b))
(equal? (remove-eq 'a '(a a a)) '())
(equal? (remove-eq 'a '(b a a)) '(b))
(equal? (remove-eq 'a '(c b a)) '(c b))
