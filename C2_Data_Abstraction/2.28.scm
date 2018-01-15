(require eopl)

(define (identifier? x)
  (and (symbol? x)
       (not (eqv? x 'lambda))))

(define-datatype lc-exp lc-exp?
  (var-exp (var identifier?))
  (lambda-exp (bound-var identifier?)
              (body lc-exp?))
  (app-exp (rator lc-exp?)
           (rand lc-exp?)))

(define (unparse exp)
  (cases lc-exp exp
    (var-exp (var) (symbol->string var))
    (lambda-exp (bound-var body)
                (format "(lambda (~a) ~a)" bound-var (unparse body)))
    (app-exp (rator rand)
             (format "(~a ~a)" (unparse rator) (unparse rand)))))

(unparse (lambda-exp 'y
                     (lambda-exp 'z
                                 (app-exp (var-exp 'x)
                                          (app-exp (var-exp 'y)
                                                   (var-exp 'z))))))
