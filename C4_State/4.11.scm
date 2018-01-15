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

(define (value-of-program pgm)
  (initialize-store!)
  (cases program pgm
    (a-program (exp)
               (value-of exp (init-env)))))

(define (empty-store)
  (make-vector 0))

(define the-store 'uninitialized)

(define (get-store)
  the-store)

(define (initialize-store!)
  (set! the-store (empty-store)))

(define (reference? v)
  (integer? v))

(define (newref val)
  (let* ((next-ref (vector-length the-store))
         (next-store (make-vector (+ next-ref 1) val)))
    (define (newref-rec idx)
      (if (equal? idx next-ref)
          0
          (begin (vector-set! next-store idx (vector-ref the-store idx))
                 (newref-rec (+ idx 1)))))
    (newref-rec 0)
    (set! the-store next-store)
    next-ref))

(define (deref ref)
  (vector-ref the-store ref))

(define (setref! ref val)
  (vector-set! the-store ref val)
  ref)

(define-datatype expval expval?
  (num-val
   (num number?))
  (bool-val
   (bool boolean?))
  (list-val
   (lst list?))
  (proc-val
   (proc proc?))
  (ref-val
   (ref reference?)))

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

(define expval->ref
  (lambda (val)
    (cases expval val
      (ref-val (ref) ref)
      (else (report-expval-extractor-error 'ref val)))))

(define expval->val
  (lambda (val)
    (cases expval val
      (num-val (num) num)
      (bool-val (bool) bool)
      (list-val (lst) (map expval->val lst))
      (proc-val (proc) proc)
      (ref-val (ref) ref))))

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

(define init-env
  (lambda ()
    (empty-nameless-env)))

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
    (expression ("-" "(" expression "," expression ")") sub-exp)
    (expression ("*" "(" expression "," expression ")") mul-exp)
    (expression ("/" "(" expression "," expression ")") div-exp)
    (expression ("minus" "(" expression ")") minus-exp)
    (expression ("zero?" "(" expression ")") zero?-exp)
    (expression ("equal?" "(" expression "," expression ")") equal?-exp)
    (expression ("less?" "(" expression "," expression ")") less?-exp)
    (expression ("greater?" "(" expression "," expression ")") greater?-exp)
    (expression ("if" expression "then" expression "else" expression) if-exp)
    (expression ("cond" (arbno expression "==>" expression) "end") cond-exp)
    (expression ("cons" "(" expression "," expression ")") cons-exp)
    (expression ("car" "(" expression ")") car-exp)
    (expression ("cdr" "(" expression ")") cdr-exp)
    (expression ("list" "(" (arbno expression) ")") list-exp)
    (expression ("null?" "(" expression ")") null?-exp)
    (expression ("emptylist") emptylist-exp)
    (expression ("unpack" (arbno identifier) "=" expression "in" expression) unpack-exp)
    (expression ("let" (arbno identifier "=" expression) "in" expression) let-exp)
    (expression ("proc" "(" (arbno identifier) ")" expression) proc-exp)
    (expression ("letrec" (arbno identifier "(" (arbno identifier) ")" "=" expression )
                 "in" expression) letrec-exp)
    (expression ("newref" "(" expression ")") newref-exp)
    (expression ("deref" "(" expression ")") deref-exp)
    (expression ("setref" "(" expression "," expression ")") setref-exp)
    (expression ("begin" expression (arbno ";" expression) "end") begin-exp)
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
  (sub-exp
   (exp1 expression?)
   (exp2 expression?))
  (mul-exp
   (exp1 expression?)
   (exp2 expression?))
  (div-exp
   (exp1 expression?)
   (exp2 expression?))
  (minus-exp
   (exp1 expression?))
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
  (car-exp
   (exp1 expression?))
  (cdr-exp
   (exp1 expression?))
  (list-exp
   (exps (list-of expression?)))
  (null?-exp
   (exp1 expression?))
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
  (newref-exp
   (exp expression?))
  (deref-exp
   (var expression?))
  (nameless-deref-exp
   (var expression?))
  (setref-exp
   (var expression?)
   (exp expression?))
  (nameless-setref-exp
   (var expression?)
   (exp expression?))
  (begin-exp
    (exp expression?)
    (exps (list-of expression?)))
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
      (sub-exp (exp1 exp2)
               (sub-exp (translation-of exp1 senv)
                        (translation-of exp2 senv)))
      (mul-exp (exp1 exp2)
               (mul-exp (translation-of exp1 senv)
                        (translation-of exp2 senv)))
      (div-exp (exp1 exp2)
               (div-exp (translation-of exp1 senv)
                        (translation-of exp2 senv)))
      (minus-exp (exp1)
                 (minus-exp (translation-of exp1 senv)))
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
      (car-exp (exp1)
               (car-exp (translation-of exp1 senv)))
      (cdr-exp (exp1)
               (cdr-exp (translation-of exp1 senv)))
      (list-exp (exps)
                (list-exp (map (lambda (exp) (translation-of exp senv)) exps)))
      (null?-exp (exp1)
                 (null?-exp (translation-of exp1 senv)))
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
      (newref-exp (exp)
                  (newref-exp (translation-of exp senv)))
      (deref-exp (var)
                 (nameless-deref-exp (translation-of var senv)))
      (setref-exp (var exp)
                  (nameless-setref-exp (translation-of var senv)
                                       (translation-of exp senv)))
      (begin-exp (exp exps)
                 (begin-exp (translation-of exp senv)
                            (map (lambda (exp) (translation-of exp senv)) exps)))
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
      (sub-exp (exp1 exp2)
               (let ((val1 (value-of exp1 nameless-env))
                     (val2 (value-of exp2 nameless-env)))
                 (let ((num1 (expval->num val1))
                       (num2 (expval->num val2)))
                   (num-val
                    (- num1 num2)))))
      (mul-exp (exp1 exp2)
               (let ((val1 (value-of exp1 nameless-env))
                     (val2 (value-of exp2 nameless-env)))
                 (let ((num1 (expval->num val1))
                       (num2 (expval->num val2)))
                   (num-val
                    (* num1 num2)))))
      (div-exp (exp1 exp2)
               (let ((val1 (value-of exp1 nameless-env))
                     (val2 (value-of exp2 nameless-env)))
                 (let ((num1 (expval->num val1))
                       (num2 (expval->num val2)))
                   (num-val
                    (quotient num1 num2)))))
      (minus-exp (exp1)
                 (let ((val1 (value-of exp1 nameless-env)))
                   (let ((num1 (expval->num val1)))
                     (num-val (- num1)))))
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
      (car-exp (exp1)
               (let ((val1 (value-of exp1 nameless-env)))
                 (let ((lst1 (expval->list val1)))
                   (car lst1))))
      (cdr-exp (exp1)
               (let ((val1 (value-of exp1 nameless-env)))
                 (let ((lst1 (expval->list val1)))
                   (list-val (cdr lst1)))))
      (list-exp (exps)
                (list-val (map (lambda (exp) (value-of exp nameless-env)) exps)))
      (null?-exp (exp1)
                 (let ((val1 (value-of exp1 nameless-env)))
                   (let ((lst1 (expval->list val1)))
                     (bool-val (null? lst1)))))
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
      (newref-exp (exp)
                  (let* ((val (value-of exp nameless-env))
                         (ref (newref val)))
                    (ref-val ref)))
      (nameless-deref-exp (var)
                          (deref (expval->ref (value-of var nameless-env))))
      (nameless-setref-exp (var exp)
                           (let* ((ref (expval->ref (value-of var nameless-env)))
                                  (val (value-of exp nameless-env)))
                             (setref! ref val)))
      (begin-exp (exp exps)
                 (let ((last (value-of exp nameless-env)))
                   (define (begin-exp-rec exps last)
                     (if (null? exps)
                         last
                         (let ((last (value-of (car exps) nameless-env)))
                           (begin-exp-rec (cdr exps) last))))
                   (begin-exp-rec exps last)))
      (else (error 'value-of "Invalid expression: " exp)))))

(define program-4
  "let x = newref(0)
   in let y = list(setref(x, 1) deref(x))
      in car(cdr(y))")
(equal? (expval->val (run program-4)) 1)
