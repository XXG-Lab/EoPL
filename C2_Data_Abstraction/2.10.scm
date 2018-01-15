(define empty-env
  (lambda () '()))

(define extend-env
  (lambda (var val env)
    (cons (list var val) env)))

(define report-no-binding-found
  (lambda (search-var)
    (error 'apply-env "No binding for ~s" search-var)))

(define apply-env
  (lambda (env search-var)
    (if (null? env)
        (report-no-binding-found search-var)
        (if (eqv? search-var (caar env))
            (cadar env)
            (apply-env (cdr env) search-var)))))

(define extend-env*
  (lambda (env var-list val-list)
    (if (null? var-list)
        env
        (extend-env* (extend-env (car var-list) (car val-list) env)
                     (cdr var-list) (cdr val-list)))))
