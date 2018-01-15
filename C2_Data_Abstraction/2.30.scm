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
    ((symbol? datum)
     (if (eqv? datum 'lambda)
         (eopl:error 'parse "Identifier 'lambda' is invalid.")
         (var-exp datum)))
    ((pair? datum)
     (if (eqv? (car datum) 'lambda)
         (if (= (length datum) 3)
             (if (list? (cadr datum))
                 (lambda-exp
                  (cadr datum)
                  (parse (caddr datum)))
                 (eopl:error 'parse "The second element should a list: ~s" datum))
             (eopl:error 'parse "Lambda should have exactly three elements: ~s" datum))
         (app-exp
          (parse (car datum))
          (map parse (cdr datum)))))
    (else (error 'parse))))
