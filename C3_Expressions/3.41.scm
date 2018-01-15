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
  (lambda (vars senv)
    (cons (list vars 'value) senv)))

(define extend-senv-letrec
  (lambda (vars senv)
    (cons (list vars 'letrec) senv)))

(define apply-senv
  (lambda (senv var)
    (define (apply-senv-sub pos vars)
      (if (null? vars)
          (let ((result (apply-senv (cdr senv) var)))
            (cons (+ (car result) 1) (cdr result)))
          (if (eqv? (car vars) var)
              (list 0 pos (cadar senv))
              (apply-senv-sub (+ pos 1) (cdr vars)))))
    (if (null? senv)
        (error 'apply-senv "Unbound variable: " var)
        (apply-senv-sub 0 (caar senv)))))

(define nameless-environment?
  (lambda (x)
    ((list-of (list-of expval?)) x)))

(define empty-nameless-env
  (lambda ()
    '()))

(define extend-nameless-env
  (lambda (vals nameless-env)
    (cons vals nameless-env)))

(define apply-nameless-env
  (lambda (nameless-env n pos)
    (list-ref (list-ref nameless-env n) pos)))

(define restore-nameless-env
  (lambda (nameless-env n)
    (if (zero? n)
        nameless-env
        (restore-nameless-env (cdr nameless-env) (- n 1)))))

(define-datatype proc proc?
  (procedure
   (body expression?)
   (saved-nameless-env nameless-environment?)))

(define apply-procedure
  (lambda (proc1 vals)
    (cases proc proc1
      (procedure (body saved-nameless-env)
                 (value-of body
                           (extend-nameless-env vals saved-nameless-env))))))

(define grammar
  '((expression (number) const-exp)
    (expression (identifier) var-exp)
    (expression ("+" "(" expression "," expression ")") add-exp)
    (expression ("-" "(" expression "," expression ")") diff-exp)
    (expression ("zero?" "(" expression ")") zero?-exp)
    (expression ("equal?" "(" expression "," expression ")") equal?-exp)
    (expression ("less?" "(" expression "," expression ")") less?-exp)
    (expression ("greater?" "(" expression "," expression ")") greater?-exp)
    (expression ("if" expression "then" expression "else" expression) if-exp)
    (expression ("cond" (arbno expression "==>" expression) "end") cond-exp)
    (expression ("cons" "(" expression "," expression ")") cons-exp)
    (expression ("emptylist") emptylist-exp)
    (expression ("unpack" (arbno identifier) "=" expression "in" expression) unpack-exp)
    (expression ("let" (arbno identifier "=" expression) "in" expression) let-exp)
    (expression ("proc" "(" (arbno identifier) ")" expression) proc-exp)
    (expression ("letrec" (arbno identifier "(" (arbno identifier) ")" "=" expression )
                 "in" expression) letrec-exp)
    (expression ("(" expression (arbno expression) ")") call-exp)))

(define (nameless-var-exp? var)
  (integer? var))

(define-datatype expression expression?
  (const-exp
   (num number?))
  (var-exp
   (var identifier?))
  (nameless-var-exp
   (n nameless-var-exp?)
   (pos nameless-var-exp?))
  (add-exp
   (exp1 expression?)
   (exp2 expression?))
  (diff-exp
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
  (cond-exp
   (conds (list-of expression?))
   (exps (list-of expression?)))
  (cons-exp
   (exp1 expression?)
   (exp2 expression?))
  (emptylist-exp)
  (unpack-exp
   (vars (list-of identifier?))
   (exp1 expression?)
   (exp2 expression?))
  (nameless-unpack-exp
   (exp1 expression?)
   (exp2 expression?))
  (let-exp
   (vars (list-of identifier?))
   (exps (list-of expression?))
   (body expression?))
  (nameless-let-exp
   (exps (list-of expression?))
   (body expression?))
  (proc-exp
   (vars (list-of identifier?))
   (body expression?))
  (letrec-exp
   (names (list-of identifier?))
   (varss (list-of (list-of identifier?)))
   (exps (list-of expression?))
   (body expression?))
  (nameless-letrec-exp
   (exps (list-of expression?))
   (body expression?))
  (nameless-letrec-var-exp
   (n nameless-var-exp?)
   (pos nameless-var-exp?))
  (nameless-proc-exp
   (body expression?))
  (call-exp
   (rator expression?)
   (rands (list-of expression?))))

(define translation-of
  (lambda (exp senv)
    (cases expression exp
      (const-exp (num) (const-exp num))
      (var-exp (var)
               (let ((result (apply-senv senv var)))
                 (if (eqv? (caddr result) 'letrec)
                     (nameless-letrec-var-exp (car result) (cadr result))
                     (nameless-var-exp (car result) (cadr result)))))
      (add-exp (exp1 exp2)
               (add-exp (translation-of exp1 senv)
                        (translation-of exp2 senv)))
      (diff-exp (exp1 exp2)
                (diff-exp (translation-of exp1 senv)
                          (translation-of exp2 senv)))
      (zero?-exp (exp1)
                 (zero?-exp (translation-of exp1 senv)))
      (equal?-exp (exp1 exp2)
                  (equal?-exp (translation-of exp1 senv)
                              (translation-of exp2 senv)))
      (less?-exp (exp1 exp2)
                 (less?-exp (translation-of exp1 senv)
                            (translation-of exp2 senv)))
      (greater?-exp (exp1 exp2)
                    (greater?-exp (translation-of exp1 senv)
                                  (translation-of exp2 senv)))
      (if-exp (exp1 exp2 exp3)
              (if-exp (translation-of exp1 senv)
                      (translation-of exp2 senv)
                      (translation-of exp3 senv)))
      (cond-exp (conds exps)
                (cond-exp (map (lambda (exp) (translation-of exp senv)) conds)
                          (map (lambda (exp) (translation-of exp senv)) exps)))
      (cons-exp (exp1 exp2)
                (cons-exp (translation-of exp1 senv)
                          (translation-of exp2 senv)))
      (emptylist-exp () (emptylist-exp))
      (unpack-exp (vars exp1 exp2)
                  (let ((trans1 (translation-of exp1 senv)))
                    (define (extend-senv-rec vars senv)
                      (if (null? vars)
                          senv
                          (extend-senv-rec (cdr vars) (extend-senv (car vars) senv))))
                    (let ((trans2 (translation-of exp2 (extend-senv-rec vars senv))))
                      (nameless-unpack-exp trans1 trans2))))
      (let-exp (vars exps body)
               (nameless-let-exp (map (lambda (exp) (translation-of exp senv)) exps)
                                 (translation-of body (extend-senv vars senv))))
      (proc-exp (vars body)
                (nameless-proc-exp (translation-of body (extend-senv vars senv))))
      (letrec-exp (names varss exps body)
                  (let ((named-env (extend-senv-letrec names senv)))
                    (define (exp-rec varss exps)
                      (if (null? exps)
                          '()
                          (cons (translation-of (car exps) (extend-senv (car varss) named-env))
                                (exp-rec (cdr varss) (cdr exps)))))
                    (nameless-letrec-exp (exp-rec varss exps)
                                         (translation-of body named-env))))
      (call-exp (rator rands)
                (call-exp
                 (translation-of rator senv)
                 (map (lambda (exp) (translation-of exp senv)) rands)))
      (else (error 'translation-of "Invalid expression: " exp)))))

(define value-of
  (lambda (exp nameless-env)
    (cases expression exp
      (const-exp (num) (num-val num))
      (add-exp (exp1 exp2)
               (let ((val1 (value-of exp1 nameless-env))
                     (val2 (value-of exp2 nameless-env)))
                 (let ((num1 (expval->num val1))
                       (num2 (expval->num val2)))
                   (num-val
                    (+ num1 num2)))))
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
      (equal?-exp (exp1 exp2)
                  (let ((val1 (value-of exp1 nameless-env))
                        (val2 (value-of exp2 nameless-env)))
                    (let ((num1 (expval->num val1))
                          (num2 (expval->num val2)))
                      (bool-val
                       (= num1 num2)))))
      (less?-exp (exp1 exp2)
                  (let ((val1 (value-of exp1 nameless-env))
                        (val2 (value-of exp2 nameless-env)))
                    (let ((num1 (expval->num val1))
                          (num2 (expval->num val2)))
                      (bool-val
                       (< num1 num2)))))
      (greater?-exp (exp1 exp2)
                  (let ((val1 (value-of exp1 nameless-env))
                        (val2 (value-of exp2 nameless-env)))
                    (let ((num1 (expval->num val1))
                          (num2 (expval->num val2)))
                      (bool-val
                       (> num1 num2)))))
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
      (cons-exp (exp1 exp2)
                (let ((val1 (value-of exp1 nameless-env))
                      (val2 (value-of exp2 nameless-env)))
                  (let ((lst2 (expval->list val2)))
                    (list-val (cons val1 lst2)))))
      (emptylist-exp ()
                     (list-val '()))
      (nameless-unpack-exp (exp1 exp2)
                  (let ((val1 (value-of exp1 nameless-env)))
                    (let ((lst1 (expval->list val1)))
                      (define (unpack-rec lst nameless-env)
                        (if (null? lst)
                            nameless-env
                            (unpack-rec (cdr lst) (extend-nameless-env (car lst) nameless-env))))
                      (value-of exp2 (unpack-rec lst1 nameless-env)))))
      (call-exp (rator rands)
                (let ((proc (expval->proc (value-of rator nameless-env)))
                      (vals (map (lambda (exp) (value-of exp nameless-env)) rands)))
                  (apply-procedure proc vals)))
      (nameless-var-exp (n pos)
                        (apply-nameless-env nameless-env n pos))
      (nameless-let-exp (exps body)
                        (let ((vals (map (lambda (exp) (value-of exp nameless-env)) exps)))
                          (value-of body (extend-nameless-env vals nameless-env))))
      (nameless-proc-exp (body)
                         (proc-val (procedure body nameless-env)))
      (nameless-letrec-exp (exps body)
                           (let ((procs (map (lambda (exp)
                                               (proc-val (procedure exp nameless-env)))
                                             exps)))
                             (value-of body (extend-nameless-env procs nameless-env))))
      (nameless-letrec-var-exp (n pos)
                               (let ((env (restore-nameless-env nameless-env n)))
                                 (let ((proc1 (expval->proc (apply-nameless-env nameless-env n pos))))
                                   (cases proc proc1
                                     (procedure (body saved-env)
                                                (proc-val (procedure body env)))))))
                               
      (else (error 'value-of "Invalid expression: " exp)))))

(equal? (expval->val (run "letrec
                             even(x) = if zero?(x) then 1 else (odd -(x,1))
                             odd(x) = if zero?(x) then 0 else (even -(x,1))
                             gcd(x y) = if equal?(x, y)
                                        then x
                                        else if greater?(x, y)
                                             then (gcd -(x, y) y)
                                             else (gcd x -(y, x))
                           in -((odd 13), (gcd 14 100))")) -1)
