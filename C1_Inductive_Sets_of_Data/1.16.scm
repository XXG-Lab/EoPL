(define invert-rec
  (lambda (lst rev)
    (if (null? lst)
        rev
        (invert-rec (cdr lst) (cons (car lst) rev)))))

(define invert
  (lambda (lst)
    (if (null? lst)
        '()
        (cons (invert-rec (car lst) '()) (invert (cdr lst))))))

(equal? (invert '((a 1 c))) '((c 1 a)))
(equal? (invert '((a 1) (a 2) (1 b) (2 b))) '((1 a) (2 a) (b 1) (b 2)))
