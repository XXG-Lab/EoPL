(define down
  (lambda (lst)
    (if (null? lst)
        '()
        (cons (cons (car lst) '())
              (down (cdr lst))))))

(equal? (down '(1 2 3)) '((1) (2) (3)))
(equal? (down '((a) (fine) (idea))) '(((a)) ((fine)) ((idea))))
(equal? (down '(a (more (complicated)) object)) '((a) ((more (complicated))) (object)))
