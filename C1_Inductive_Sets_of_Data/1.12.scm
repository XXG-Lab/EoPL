(define subst
  (lambda (new old slist)
    (if (null? slist)
        '()
        (cons
          (if (symbol? (car slist))
              (if (eqv? (car slist) old)
                  new
                  (car slist))
              (subst new old (car slist)))
          (subst new old (cdr slist))))))

(equal? (subst 'a 'b '((b c) (b () d))) '((a c) (a () d)))
