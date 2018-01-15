(require eopl)

(define scanner-spec
  '((white-sp (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier (letter (arbno (or letter digit))) symbol)
    (number (digit (arbno digit)) number)))

(define scan&parse
  (sllgen:make-string-parser scanner-spec grammar))

(define report-expval-extractor-error
  (lambda (type val)
    (error type "Extraction type error: " val)))

(define value-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
                 (value-of exp1 (empty-nameless-env))))))

(define-datatype program program?
  (a-program
   (exp1 expression?)))

(define translation-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
                 (a-program (translation-of exp1 (empty-senv)))))))

(define run
  (lambda (string)
    (value-of-program
     (translation-of-program
      (a-program
       (scan&parse string))))))

(define (identifier? x)
  (and (symbol? x)
       (not (eqv? x 'lambda))))

(define-datatype expval expval?
  (num-val
   (num number?))
  (bool-val
   (bool boolean?))
  (list-val
   (lst list?))
  (proc-val
   (proc proc?)))

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

(define expval->proc
  (lambda (val)
    (cases expval val
      (proc-val (proc) proc)
      (else (report-expval-extractor-error 'proc val)))))

(define expval->val
  (lambda (val)
    (cases expval val
      (num-val (num) num)
      (bool-val (bool) bool)
      (list-val (lst) (map expval->val lst))
      (proc-val (proc) proc))))


(define empty-senv
  (lambda ()
    '()))

(define extend-senv
  (lambda (var senv)
    (cons var senv)))

(define apply-senv
  (lambda (senv var)
    (cond ((null? senv) (error 'apply-senv "Unbound variable: " var))
          ((eqv? var (car senv)) 0)
          (else (+ 1 (apply-senv (cdr senv) var))))))

(define nameless-environment?
  (lambda (x)
    ((list-of expval?) x)))

(define empty-nameless-env
  (lambda ()
    '()))

(define extend-nameless-env
  (lambda (val nameless-env)
    (cons val nameless-env)))

(define apply-nameless-env
  (lambda (nameless-env n)
    (list-ref nameless-env n)))

(define-datatype proc proc?
  (procedure
   (body expression?)
   (saved-nameless-env nameless-environment?)))

(define apply-procedure
  (lambda (proc1 val)
    (cases proc proc1
      (procedure (body saved-nameless-env)
                 (value-of body
                           (extend-nameless-env val saved-nameless-env))))))

(define grammar
  '((expression (number) const-exp)
    (expression (identifier) var-exp)
    (expression ("-" "(" expression "," expression ")") diff-exp)
    (expression ("zero?" "(" expression ")") zero?-exp)
    (expression ("if" expression "then" expression "else" expression) if-exp)
    (expression ("cond" (arbno expression "==>" expression) "end") cond-exp)
    (expression ("let" identifier "=" expression "in" expression) let-exp)
    (expression ("proc" "(" identifier ")" expression) proc-exp)
    (expression ("(" expression expression ")") call-exp)))

(define (nameless-var-exp? var)
  (integer? var))

(define-datatype expression expression?
  (const-exp
   (num number?))
  (var-exp
   (var identifier?))
  (diff-exp
   (exp1 expression?)
   (exp2 expression?))
  (zero?-exp
   (exp1 expression?))
  (if-exp
   (exp1 expression?)
   (exp2 expression?)
   (exp3 expression?))
  (cond-exp
   (conds (list-of expression?))
   (exps (list-of expression?)))
  (nameless-var-exp
   (var nameless-var-exp?))
  (let-exp
   (var identifier?)
   (exp expression?)
   (body expression?))
  (nameless-let-exp
   (exp expression?)
   (body expression?))
  (proc-exp
   (var identifier?)
   (body expression?))
  (nameless-proc-exp
   (body expression?))
  (call-exp
   (rator expression?)
   (rand expression?)))

(define translation-of
  (lambda (exp senv)
    (cases expression exp
      (const-exp (num) (const-exp num))
      (diff-exp (exp1 exp2)
                (diff-exp (translation-of exp1 senv)
                          (translation-of exp2 senv)))
      (zero?-exp (exp1)
                 (zero?-exp (translation-of exp1 senv)))
      (if-exp (exp1 exp2 exp3)
              (if-exp (translation-of exp1 senv)
                      (translation-of exp2 senv)
                      (translation-of exp3 senv)))
      (cond-exp (conds exps)
                (cond-exp (map (lambda (exp) (translation-of exp senv)) conds)
                          (map (lambda (exp) (translation-of exp senv)) exps)))
      (var-exp (var)
               (nameless-var-exp
                (apply-senv senv var)))
      (let-exp (var exp1 body)
               (nameless-let-exp (translation-of exp1 senv)
                                 (translation-of body (extend-senv var senv))))
      (proc-exp (var body)
                (nameless-proc-exp (translation-of body (extend-senv var senv))))
      (call-exp (rator rand)
                (call-exp
                 (translation-of rator senv)
                 (translation-of rand senv)))
      (else (error 'translation-of "Invalid expression: " exp)))))

(define value-of
  (lambda (exp nameless-env)
    (cases expression exp
      (const-exp (num) (num-val num))
      (diff-exp (exp1 exp2)
                (let ((val1 (value-of exp1 nameless-env))
                      (val2 (value-of exp2 nameless-env)))
                  (let ((num1 (expval->num val1))
                        (num2 (expval->num val2)))
                    (num-val
                     (- num1 num2)))))
      (zero?-exp (exp1)
                 (let ((val1 (value-of exp1 nameless-env)))
                   (let ((num1 (expval->num val1)))
                     (if (zero? num1)
                         (bool-val #t)
                         (bool-val #f)))))
      (if-exp (exp1 exp2 exp3)
              (let ((val1 (value-of exp1 nameless-env)))
                (if (expval->bool val1)
                    (value-of exp2 nameless-env)
                    (value-of exp3 nameless-env))))
      (cond-exp (conds exps)
                (cond ((null? conds) (error 'cond "No condition matched."))
                      ((expval->bool (value-of (car conds) nameless-env))
                       (value-of (car exps) nameless-env))
                      (else
                       (value-of (cond-exp (cdr conds) (cdr exps)) nameless-env))))
      (call-exp (rator rand)
                (let ((proc (expval->proc (value-of rator nameless-env)))
                      (val (value-of rand nameless-env)))
                  (apply-procedure proc val)))
      (nameless-var-exp (n)
                        (apply-nameless-env nameless-env n))
      (nameless-let-exp (exp1 body)
                        (let ((val (value-of exp1 nameless-env)))
                          (value-of body
                                    (extend-nameless-env val nameless-env))))
      (nameless-proc-exp (body)
                         (proc-val (procedure body nameless-env)))
      (else (error 'value-of "Invalid expression: " exp)))))

(equal? (expval->val (run "let f = proc(x)
                                     cond
                                       zero?(x) ==> 1
                                       zero?(0) ==> 2
                                     end
                           in (f 0)")) 1)
(equal? (expval->val (run "cond
                             zero? (1) ==> 1
                             zero? (1) ==> 2
                           end")) 2)
