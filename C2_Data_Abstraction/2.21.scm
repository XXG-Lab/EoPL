(require eopl)

(define-datatype env env?
  (empty-env)
  (extend-env (var symbol?)
              (val (lambda (x) #t))
              (old-env env?)))

(define (has-binding? search-env search-var)
  (cases env search-env
    (empty-env () #f)
    (extend-env (var val old-env)
                (or (eqv? var search-var)
                    (has-binding? old-env search-var)))))

(define search-env
  (extend-env 'd 6  (extend-env 'y 8 (extend-env 'x 7 (extend-env 'y 14 (empty-env))))))
(equal? (has-binding? search-env 'x) #t)
(equal? (has-binding? search-env 'w) #f)
