(define duple
  (lambda (n x)
    (if (zero? n)
        '()
        (cons x (duple (- n 1) x)))))

(equal? (duple 2 3) '(3 3))
(equal? (duple 4 '(ha ha)) '((ha ha) (ha ha) (ha ha) (ha ha)))
(equal? (duple 0 '(blah)) '())
