(define count-occurrences
  (lambda (s slist)
    (if (null? slist)
        0
        (if (list? slist)
            (+ (count-occurrences s (car slist)) (count-occurrences s (cdr slist)))
            (if (eqv? s slist) 1 0)))))

(equal? (count-occurrences 'x '((f x) y (((x z) x)))) 3)
(equal? (count-occurrences 'x '((f x) y (((x z) () x)))) 3)
(equal? (count-occurrences 'w '((f x) y (((x z) x)))) 0)
