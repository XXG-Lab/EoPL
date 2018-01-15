(define subst
  (lambda (new old slist)
    (if (list? slist)
        (map (lambda (lst) (subst new old lst)) slist)
        (if (symbol? slist)
            (if (eqv? old slist) new slist)
            slist))))

(equal? (subst 'a 'b '((b c) (b () d))) '((a c) (a () d)))
