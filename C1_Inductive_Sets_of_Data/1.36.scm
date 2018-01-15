(define g-inc
  (lambda (lst)
    (if (null? lst)
        '()
        (cons (cons (+ (caar lst) 1) (cdar lst))
              (g-inc (cdr lst))))))

(define g
  (lambda (p lst)
    (cons p (g-inc lst))))

(define number-elements
  (lambda (lst)
    (if (null? lst) '()
        (g (list 0 (car lst)) (number-elements (cdr lst))))))

(equal? (number-elements '(a b c)) '((0 a) (1 b) (2 c)))
