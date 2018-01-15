(define (var-exp var) (list 'var var))
(define (var-exp? exp) (eqv? 'var (car exp)))
(define (var-exp->var exp) (cadr exp))

(define (lambda-exp bound-var body) (list 'lambda bound-var body))
(define (lambda-exp? exp) (eqv? 'lambda (car exp)))
(define (lambda-exp->bound-var exp) (var-exp->var (caadr exp)))
(define (lambda-exp->body exp) (caddr exp))
  
(define (app-exp rator rand) (list 'app rator rand))
(define (app-exp? exp) (eqv? 'app (car exp)))
(define (app-exp->rator exp) (cadr exp))
(define (app-exp->rand exp) (caddr exp))

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
(equal? (occurs-free? 'x (lambda-exp (list (var-exp 'x)) (app-exp (var-exp 'x) (var-exp 'y)))) #f)
(equal? (occurs-free? 'x (lambda-exp (list (var-exp 'y))
                                     (app-exp (var-exp 'x) (var-exp 'y)))) #t)
(equal? (occurs-free? 'x (app-exp (lambda-exp (list (var-exp 'x)) (var-exp 'x))
                                  (app-exp (var-exp 'x) (var-exp 'y)))) #t)
(equal? (occurs-free? 'x (lambda-exp (list (var-exp 'y))
                                     (lambda-exp (list (var-exp 'z))
                                                 (app-exp (var-exp 'x)
                                                          (app-exp (var-exp 'y) (var-exp 'z)))))) #t)
