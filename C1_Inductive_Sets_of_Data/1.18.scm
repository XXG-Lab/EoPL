(define swapper
  (lambda (s1 s2 slist)
    (if (null? slist)
        '()
        (if (symbol? slist)
            (if (eqv? s1 slist)
                s2
                (if (eqv? s2 slist)
                    s1
                    slist))
            (if (list? slist)
                (cons (swapper s1 s2 (car slist)) (swapper s1 s2 (cdr slist)))
                slist)))))

(equal? (swapper 'a 'd '(a b c d)) '(d b c a))
(equal? (swapper 'a 'd '(a d () c d)) '(d a () c a))
(equal? (swapper 'x 'y '((x) y (z (x)))) '((y) x (z (y))))
