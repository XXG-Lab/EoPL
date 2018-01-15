(require eopl)

(define-datatype program program?
  (a-program
   (exp1 expression?)))

(define (identifier? x)
  (and (symbol? x)
       (not (eqv? x 'lambda))))

(define-datatype expval expval?
  (num-val
   (num number?))
  (bool-val
   (bool boolean?))
  (list-val
   (lst list?)))

(define report-expval-extractor-error
  (lambda (type val)
    (error type "Extraction type error: " val)))

(define expval->num
  (lambda (val)
    (cases expval val
      (num-val (num) num)
      (else (report-expval-extractor-error 'num val)))))

(define expval->bool
  (lambda (val)
    (cases expval val
      (bool-val (bool) bool)
      (else (report-expval-extractor-error 'bool val)))))

(define expval->list
  (lambda (val)
    (cases expval val
      (list-val (lst) lst)
      (else (report-expval-extractor-error 'list val)))))

(define expval->val
  (lambda (val)
    (cases expval val
      (num-val (num) num)
      (bool-val (bool) bool)
      (list-val (lst) (map expval->val lst)))))

(define run
  (lambda (string)
    (value-of-program (a-program (scan&parse string)))))

(define value-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
                 (value-of exp1 (empty-env))))))

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

(define scanner-spec
  '((white-sp (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier (letter (arbno (or letter digit))) symbol)
    (number (digit (arbno digit)) number)))

(define scan&parse
  (sllgen:make-string-parser scanner-spec grammar))

(define grammar
  '((expression (number) const-exp)
    (expression ("-" "(" expression "," expression ")") diff-exp)
    (expression ("+" "(" expression "," expression ")") add-exp)
    (expression ("*" "(" expression "," expression ")") mul-exp)
    (expression ("/" "(" expression "," expression ")") quotient-exp)
    (expression ("zero?" "(" expression ")") zero?-exp)
    (expression ("equal?" "(" expression "," expression ")") equal?-exp)
    (expression ("less?" "(" expression "," expression ")") less?-exp)
    (expression ("greater?" "(" expression "," expression ")") greater?-exp)
    (expression ("minus" "(" expression ")") minus-exp)
    (expression ("if" expression "then" expression "else" expression) if-exp)
    (expression (identifier) var-exp)
    (expression ("let" identifier "=" expression "in" expression) let-exp)
    (expression ("cons" "(" expression "," expression ")") cons-exp)
    (expression ("car" "(" expression ")") car-exp)
    (expression ("cdr" "(" expression ")") cdr-exp)
    (expression ("null?" "(" expression ")") null?-exp)
    (expression ("emptylist") emptylist-exp)))

(define-datatype expression expression?
  (const-exp
   (num number?))
  (diff-exp
   (exp1 expression?)
   (exp2 expression?))
  (add-exp
   (exp1 expression?)
   (exp2 expression?))
  (mul-exp
   (exp1 expression?)
   (exp2 expression?))
  (quotient-exp
   (exp1 expression?)
   (exp2 expression?))
  (zero?-exp
   (exp1 expression?))
  (equal?-exp
   (exp1 expression?)
   (exp2 expression?))
  (less?-exp
   (exp1 expression?)
   (exp2 expression?))
  (greater?-exp
   (exp1 expression?)
   (exp2 expression?))
  (if-exp
   (exp1 expression?)
   (exp2 expression?)
   (exp3 expression?))
  (var-exp
   (var identifier?))
  (let-exp
   (var identifier?)
   (exp1 expression?)
   (body expression?))
  (minus-exp
   (exp1 expression?))
  (cons-exp
   (exp1 expression?)
   (exp2 expression?))
  (car-exp
   (exp1 expression?))
  (cdr-exp
   (exp1 expression?))
  (null?-exp
   (exp1 expression?))
  (emptylist-exp))

(define value-of
  (lambda (exp env)
    (cases expression exp
      (const-exp (num) (num-val num))
      (var-exp (var) (apply-env env var))
      (diff-exp (exp1 exp2)
                (let ((val1 (value-of exp1 env))
                      (val2 (value-of exp2 env)))
                  (let ((num1 (expval->num val1))
                        (num2 (expval->num val2)))
                    (num-val
                     (- num1 num2)))))
      (add-exp (exp1 exp2)
               (let ((val1 (value-of exp1 env))
                     (val2 (value-of exp2 env)))
                 (let ((num1 (expval->num val1))
                       (num2 (expval->num val2)))
                   (num-val
                    (+ num1 num2)))))
      (mul-exp (exp1 exp2)
               (let ((val1 (value-of exp1 env))
                     (val2 (value-of exp2 env)))
                 (let ((num1 (expval->num val1))
                       (num2 (expval->num val2)))
                   (num-val
                    (* num1 num2)))))
      (quotient-exp (exp1 exp2)
               (let ((val1 (value-of exp1 env))
                     (val2 (value-of exp2 env)))
                 (let ((num1 (expval->num val1))
                       (num2 (expval->num val2)))
                   (num-val
                    (quotient num1 num2)))))
      (zero?-exp (exp1)
                 (let ((val1 (value-of exp1 env)))
                   (let ((num1 (expval->num val1)))
                     (if (zero? num1)
                         (bool-val #t)
                         (bool-val #f)))))
      (equal?-exp (exp1 exp2)
                  (let ((val1 (value-of exp1 env))
                     (val2 (value-of exp2 env)))
                 (let ((num1 (expval->num val1))
                       (num2 (expval->num val2)))
                   (bool-val
                    (= num1 num2)))))
      (less?-exp (exp1 exp2)
                  (let ((val1 (value-of exp1 env))
                     (val2 (value-of exp2 env)))
                 (let ((num1 (expval->num val1))
                       (num2 (expval->num val2)))
                   (bool-val
                    (< num1 num2)))))
      (greater?-exp (exp1 exp2)
                  (let ((val1 (value-of exp1 env))
                     (val2 (value-of exp2 env)))
                 (let ((num1 (expval->num val1))
                       (num2 (expval->num val2)))
                   (bool-val
                    (> num1 num2)))))
      (if-exp (exp1 exp2 exp3)
              (let ((val1 (value-of exp1 env)))
                (if (expval->bool val1)
                    (value-of exp2 env)
                    (value-of exp3 env))))
      (let-exp (var exp1 body)
               (let ((val1 (value-of exp1 env)))
                 (value-of body
                           (extend-env var val1 env))))
      (minus-exp (exp1)
                 (num-val (- (expval->num (value-of exp1 env)))))
      (cons-exp (exp1 exp2)
                (let ((val1 (value-of exp1 env))
                      (val2 (value-of exp2 env)))
                  (let ((lst2 (expval->list val2)))
                    (list-val (cons val1 lst2)))))
      (car-exp (exp1)
               (let ((val1 (value-of exp1 env)))
                 (let ((lst1 (expval->list val1)))
                   (car lst1))))
      (cdr-exp (exp1)
               (let ((val1 (value-of exp1 env)))
                 (let ((lst1 (expval->list val1)))
                   (list-val (cdr lst1)))))
      (null?-exp (exp1)
                 (let ((val1 (value-of exp1 env)))
                   (let ((lst1 (expval->list val1)))
                     (bool-val (null? lst1)))))
      (emptylist-exp () (list-val '())))))

(equal? (expval->val (run "let x = 4
                           in cons(x,
                                   cons(cons(-(x,1),
                                             emptylist),
                                       emptylist))")) '(4 (3)))
(equal? (expval->val (run "let x = 4
                           in car(cons(x,
                                       cons(cons(-(x,1),
                                                 emptylist),
                                           emptylist)))")) 4)
(equal? (expval->val (run "let x = 4
                           in cdr(cons(x,
                                       cons(cons(-(x,1),
                                                 emptylist),
                                           emptylist)))")) '((3)))
(equal? (expval->val (run "let x = 4
                           in car(cdr(cons(x,
                                           cons(cons(-(x,1),
                                                     emptylist),
                                               emptylist))))")) '(3))
(equal? (expval->val (run "let x = 4
                           in cdr(cdr(cons(x,
                                           cons(cons(-(x,1),
                                                     emptylist),
                                               emptylist))))")) '())
(equal? (expval->val (run "let x = 4
                           in null?(car(cdr(cons(x,
                                                 cons(cons(-(x,1),
                                                           emptylist),
                                                     emptylist)))))")) #f)
(equal? (expval->val (run "let x = 4
                           in null?(cdr(cdr(cons(x,
                                                 cons(cons(-(x,1),
                                                           emptylist),
                                                     emptylist)))))")) #t)
