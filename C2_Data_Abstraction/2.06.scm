(define empty-env
  (lambda () '()))

(define extend-env
  (lambda (var val env)
    (cons var (cons val env))))

(define report-no-binding-found
  (lambda (search-var)
    (error 'apply-env "No binding for ~s" search-var)))

(define apply-env
  (lambda (env search-var)
    (if (null? env)
        (report-no-binding-found search-var)
        (if (eqv? search-var (car env))
            (cadr env)
            (apply-env (cddr env) search-var)))))

(define env
  (extend-env 'd 6
              (extend-env 'y 8
                          (extend-env 'x 7
                                      (extend-env 'y 14
                                                  (empty-env))))))
(display (apply-env env 'x))
(display (apply-env env 'w))
