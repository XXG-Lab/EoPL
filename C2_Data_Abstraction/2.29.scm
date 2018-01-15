(require eopl)

(define (identifier? x)
  (and (symbol? x)
       (not (eqv? x 'lambda))))

(define-datatype lc-exp lc-exp?
  (var-exp (var identifier?))
  (lambda-exp (bound-var (list-of identifier?))
              (body lc-exp?))
  (app-exp (rator lc-exp?)
           (rand (list-of lc-exp?))))

(define (parse datum)
  (cond
    ((symbol? datum) (var-exp datum))
    ((pair? datum)
     (if (eqv? (car datum) 'lambda)
         (lambda-exp
          (cadr datum)
          (parse (caddr datum)))
         (app-exp
          (parse (car datum))
          (map parse (cdr datum)))))
    (else (error 'parse))))

(parse '(lambda (x y) (a b c)))
