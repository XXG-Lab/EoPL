(define empty-env
  (lambda () '()))

(define extend-env
  (lambda (var val env)
    (cons (list (list var) (list val)) env)))

(define extend-env*
  (lambda (var-list val-list env)
    (cons (list var-list val-list) env)))

(define report-no-binding-found
  (lambda (search-var)
    (error 'apply-env "No binding for ~s" search-var)))

(define apply-env-sub
  (lambda (var-list val-list var)
    (if (null? var-list)
        '(#f)
        (if (eqv? var (car var-list))
            (list #t (car val-list))
            (apply-env-sub (cdr var-list) (cdr val-list) var)))))

(define apply-env
  (lambda (env search-var)
    (if (null? env)
        (report-no-binding-found search-var)
        (let ((ret (apply-env-sub (caar env) (cadar env) search-var)))
          (if (equal? (car ret) #t)
              (cadr ret)
              (apply-env (cdr env) search-var))))))

(define env (extend-env 'd 4 (extend-env* '(a b c) '(1 2 3) (empty-env))))
(equal? (apply-env env 'd) 4)
(equal? (apply-env env 'b) 2)
