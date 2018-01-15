(define every?
  (lambda (pred lst)
    (if (null? lst)
        #t
        (if (pred (car lst))
            (every? pred (cdr lst))
            #f))))

(equal? (every? number? '(a b c 3 e)) #f)
(equal? (every? number? '(1 2 3 5 4)) #t)
