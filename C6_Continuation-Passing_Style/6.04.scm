(require eopl)

(define any?
  (lambda (x) #t))

(define-datatype continuation continuation?
  (app-exp-1-cont (search-var symbol?)
                  (exp any?)
                  (saved-cont continuation?))
  (app-exp-2-cont (val1 any?)
                  (saved-cont continuation?))
  (subst-cont (new symbol?)
              (old symbol?)
              (fst any?)
              (lst any?)
              (saved-cont continuation?))
  (end-cont))

(define (apply-cont cont val)
  (cases continuation cont
    (app-exp-1-cont (search-var exp saved-cont)
                    (occurs-free?/k search-var exp (app-exp-2-cont val saved-cont)))
    (app-exp-2-cont (val1 saved-cont)
                    (apply-cont saved-cont (or val1 val)))
    (subst-cont (new old fst lst saved-cont)
                (subst/k new old (cons val fst) lst saved-cont))
    (end-cont () val)))

; remove-first

(define remove-first
  (lambda (s lst)
    (remove-first/k s lst '() (end-cont))))

(define remove-first/k
  (lambda (s lst fst cont)
    (if (null? lst)
        (apply-cont cont (reverse fst))
        (if (eqv? s (car lst))
            (apply-cont cont (append (reverse fst) (cdr lst)))
            (remove-first/k s (cdr lst) (cons (car lst) fst) cont)))))

(equal? (remove-first 1 '(1 2 3)) '(2 3))
(equal? (remove-first 2 '(1 2 3)) '(1 3))
(equal? (remove-first 3 '(1 2 3)) '(1 2))
(equal? (remove-first 4 '(1 2 3)) '(1 2 3))
(equal? (remove-first 1 '(1 1 1)) '(1 1))

; list-sum

(define list-sum
  (lambda (lst)
    (list-sum/k 0 lst (end-cont))))

(define list-sum/k
  (lambda (val lst cont)
    (if (null? lst)
        (apply-cont cont val)
        (list-sum/k (+ val (car lst)) (cdr lst) cont))))

(equal? (list-sum '(1 2 3)) 6)
(equal? (list-sum '()) 0)

; occurs-free?

(define (var-exp var) var)
(define (var-exp? exp) (symbol? exp))
(define (var-exp->var exp) exp)

(define (lambda-exp bound-var body) (list 'lambda (list bound-var) body))
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
    (occurs-free?/k search-var exp (end-cont))))

(define occurs-free?/k
  (lambda (search-var exp cont)
    (cond ((var-exp? exp)
           (apply-cont cont (eqv? search-var (var-exp->var exp))))
          ((lambda-exp? exp)
           (if (eqv? search-var (lambda-exp->bound-var exp))
               (apply-cont cont #f)
               (occurs-free?/k search-var (lambda-exp->body exp) cont)))
          (else
           (occurs-free?/k search-var (app-exp->rator exp)
                           (app-exp-1-cont search-var (app-exp->rand exp) cont))))))

(equal? (occurs-free? 'x (var-exp 'x)) #t)
(equal? (occurs-free? 'x (var-exp 'y)) #f)
(equal? (occurs-free? 'x (lambda-exp 'x '(x y))) #f)
(equal? (occurs-free? 'x (lambda-exp 'y '(x y))) #t)
(equal? (occurs-free? 'x (app-exp (lambda-exp 'x 'x) (app-exp 'x 'y))) #t)
(equal? (occurs-free? 'x (lambda-exp 'y (lambda-exp 'z (app-exp 'x (app-exp 'y 'z))))) #t)
  
; subst

(define subst
  (lambda (new old lst)
    (subst/k new old '() lst (end-cont))))

(define subst/k
 (lambda (new old fst lst cont)
   (if (null? lst)
       (apply-cont cont (reverse fst))
       (subst-in-s-exp/k new old (car lst)
                         (subst-cont new old fst (cdr lst) cont)))))

(define subst-in-s-exp/k
 (lambda (new old sexp cont)
   (if (symbol? sexp)
       (if (eqv? sexp old)
           (apply-cont cont new)
           (apply-cont cont sexp))
       (subst/k new old '() sexp cont))))

(equal? (subst 'a 'b '(a b c)) '(a a c))
(equal? (subst 'a 'b '(() ((b)))) '(() ((a))))
