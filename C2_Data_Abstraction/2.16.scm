(define (var-exp var) var)
(define (var-exp? exp) (symbol? exp))
(define (var-exp->var exp) exp)

(define (lambda-exp bound-var body) (list 'lambda bound-var body))
(define (lambda-exp? exp) (and (list? exp)
                               (eqv? 'lambda (car exp))))
(define (lambda-exp->bound-var exp) (caadr exp))
(define (lambda-exp->body exp) (caddr exp))
  
(define (app-exp rator rand) (list rator rand))
(define (app-exp? exp) (and (list? exp)
                            (not eqv? 'lambda (car exp))))
(define (app-exp->rator exp) (car exp))
(define (app-exp->rand exp) (cadr exp))

(define occurs-free?
  (lambda (search-var exp)
    (cond
      ((var-exp? exp) (eqv? search-var (var-exp->var exp)))
      ((lambda-exp? exp)
       (and
        (not (eqv? search-var (lambda-exp->bound-var exp)))
        (occurs-free? search-var (lambda-exp->body exp))))
      (else
       (or
        (occurs-free? search-var (app-exp->rator exp))
        (occurs-free? search-var (app-exp->rand exp)))))))

(equal? (occurs-free? 'x (var-exp 'x)) #t)
(equal? (occurs-free? 'x (var-exp 'y)) #f)
(equal? (occurs-free? 'x (lambda-exp '(x) '(x y))) #f)
(equal? (occurs-free? 'x (lambda-exp '(y) '(x y))) #t)
(equal? (occurs-free? 'x (app-exp (lambda-exp '(x) 'x) (app-exp 'x 'y))) #t)
(equal? (occurs-free? 'x (lambda-exp '(y) (lambda-exp '(z) (app-exp 'x (app-exp 'y 'z))))) #t)
