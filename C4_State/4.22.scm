(require eopl)

(define scanner-spec
  '((white-sp (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier (letter (arbno (or letter digit))) symbol)
    (number (digit (arbno digit)) number)
    (number ("-" digit (arbno digit)) number)))

(define scan&parse
  (sllgen:make-string-parser scanner-spec grammar))

(define report-expval-extractor-error
  (lambda (type val)
    (error type "Extraction type error: " val)))

(define-datatype program program?
  (a-program
   (stat statement?)))

(define translation-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (stat)
                 (a-program (translation-of-stat stat (empty-senv)))))))

(define translate
  (lambda (string)
    (translation-of-program
      (scan&parse string))))

(define run
  (lambda (string)
    (car
     (value-of-program
      (translate string)))))
  
(define (value-of-program pgm)
  (cases program pgm
    (a-program (stat)
               (result-of stat (empty-env) (empty-store)))))

(define (identifier? x)
  (and (symbol? x)
       (not (eqv? x 'lambda))))

(define (reference? v)
  (integer? v))

(define (store? x)
  ((list-of expval?) x))

(define (empty-store)
  '())

(define (newref store val)
  (let* ((next-ref (length store))
         (next-store (append store (list val))))
    (list next-ref next-store)))

(define (deref store ref)
  (list-ref store ref))

(define (setref store ref val)
  (define (setref-rec store-rec ref-rec)
    (cond ((null? store-rec) (error 'setref "Invalid reference: " ref store))
          ((zero? ref-rec) (cons val (cdr store-rec)))
          (else (cons (car store-rec)
                      (setref-rec (cdr store-rec)
                                  (- ref-rec 1))))))
  (setref-rec store ref))

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
      (ref-val (ref) (list 'ref ref)))))

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

(define environment?
  (lambda (x)
    ((list-of (list-of reference?)) x)))

(define empty-env
  (lambda ()
    '()))

(define extend-env
  (lambda (vals env)
    (cons vals env)))

(define extend-env-ref
  (lambda (vals env store)
    (define (extend-env-ref-rec vals store)
      (if (null? vals)
          (list '() store)
          (let* ((ret (newref store (car vals)))
                 (val (car ret))
                 (next-store (cadr ret))
                 (rest-ret (extend-env-ref-rec (cdr vals) next-store))
                 (rest-val (car rest-ret))
                 (rest-store (cadr rest-ret)))
            (list (cons val rest-val) rest-store))))
    (let* ((ret (extend-env-ref-rec vals store))
           (vals (car ret))
           (next-store (cadr ret)))
      (list (extend-env vals env) next-store))))

(define apply-env
  (lambda (env n pos)
    (list-ref (list-ref env n) pos)))

(define apply-env-ref
  (lambda (env n pos store)
    (deref store (apply-env env n pos))))

(define restore-env
  (lambda (env n)
    (if (zero? n)
        env
        (restore-env (cdr env) (- n 1)))))

(define-datatype proc proc?
  (procedure
   (body expression?)
   (saved-env environment?)))

(define (apply-procedure proc1 vals store)
  (cases proc proc1
    (procedure (body saved-env)
               (let* ((ret (extend-env-ref vals saved-env store))
                      (next-env (car ret))
                      (next-store (cadr ret)))
                 (value-of body next-env next-store)))))

(define grammar
  '((program (statement) a-program)
    (expression (number) const-exp)
    (expression (identifier) var-exp)
    (expression ("+" "(" expression "," expression ")") add-exp)
    (expression ("-" "(" expression "," expression ")") sub-exp)
    (expression ("*" "(" expression "," expression ")") mul-exp)
    (expression ("/" "(" expression "," expression ")") div-exp)
    (expression ("minus" "(" expression ")") minus-exp)
    (expression ("not" "(" expression ")") not-exp)
    (expression ("and" "(" expression "," expression ")") and-exp)
    (expression ("or" "(" expression "," expression ")") or-exp)
    (expression ("zero?" "(" expression ")") zero?-exp)
    (expression ("equal?" "(" expression "," expression ")") equal?-exp)
    (expression ("less?" "(" expression "," expression ")") less?-exp)
    (expression ("greater?" "(" expression "," expression ")") greater?-exp)
    (expression ("cons" "(" expression "," expression ")") cons-exp)
    (expression ("car" "(" expression ")") car-exp)
    (expression ("cdr" "(" expression ")") cdr-exp)
    (expression ("list" "(" (arbno expression) ")") list-exp)
    (expression ("null?" "(" expression ")") null?-exp)
    (expression ("emptylist") emptylist-exp)
    (expression ("unpack" (arbno identifier) "=" expression "in" expression) unpack-exp)
    (expression ("let" (arbno identifier "=" expression) "in" expression) let-exp)
    (expression ("let*" (arbno identifier "=" expression) "in" expression) let*-exp)
    (expression ("proc" "(" (arbno identifier) ")" expression) proc-exp)
    (expression ("letproc" (arbno identifier "(" (arbno identifier) ")" "=" expression)
                 "in" expression) letproc-exp)
    (expression ("letrec" (arbno identifier "(" (arbno identifier) ")" "=" expression )
                 "in" expression) letrec-exp)
    (expression ("newref" "(" expression ")") newref-exp)
    (expression ("deref" "(" expression ")") deref-exp)
    (expression ("setref" "(" expression "," expression ")") setref-exp)
    (expression ("begin" expression (arbno ";" expression) "end") begin-exp)
    (expression ("set" identifier "=" expression) assign-exp)
    (expression ("setdynamic" (arbno identifier "=" expression) "during" expression) setdynamic-exp)
    (expression ("(" expression (arbno expression) ")") call-exp)
    (statement (identifier "=" expression) assign-stat)
    (statement ("print" expression) print-stat)
    (statement ("{" (separated-list statement ";") "}") seq-stat)
    (statement ("if" expression statement statement) if-stat)
    (statement ("cond" (arbno expression "==>" statement) "end") cond-stat)
    (statement ("while" expression statement) while-stat)
    (statement ("var" (separated-list identifier ",") ";" statement) var-stat)))

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
   (exp expression?))
  (not-exp
   (exp expression?))
  (and-exp
   (exp1 expression?)
   (exp2 expression?))
  (or-exp
   (exp1 expression?)
   (exp2 expression?))
  (zero?-exp
   (exp expression?))
  (equal?-exp
   (exp1 expression?)
   (exp2 expression?))
  (less?-exp
   (exp1 expression?)
   (exp2 expression?))
  (greater?-exp
   (exp1 expression?)
   (exp2 expression?))
  (cons-exp
   (exp1 expression?)
   (exp2 expression?))
  (car-exp
   (exp expression?))
  (cdr-exp
   (exp expression?))
  (list-exp
   (exps (list-of expression?)))
  (null?-exp
   (exp expression?))
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
  (let*-exp
   (vars (list-of identifier?))
   (exps (list-of expression?))
   (body expression?))
  (nameless-let-exp
   (exps (list-of expression?))
   (body expression?))
  (proc-exp
   (vars (list-of identifier?))
   (body expression?))
  (letproc-exp
   (names (list-of identifier?))
   (varss (list-of (list-of identifier?)))
   (exps (list-of expression?))
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
  (assign-exp
   (var identifier?)
   (exp expression?))
  (nameless-assign-exp
   (n nameless-var-exp?)
   (pos nameless-var-exp?)
   (exp expression?))
  (setdynamic-exp
   (vars (list-of identifier?))
   (exps (list-of expression?))
   (body expression?))
  (nameless-setdynamic-exp
   (n nameless-var-exp?)
   (pos nameless-var-exp?)
   (exp expression?)
   (body expression?))
  (call-exp
   (rator expression?)
   (rands (list-of expression?))))

(define-datatype statement statement?
  (assign-stat
   (var identifier?)
   (exp expression?))
  (nameless-assign-stat
   (n nameless-var-exp?)
   (pos nameless-var-exp?)
   (exp expression?))
  (print-stat
   (exp expression?))
  (seq-stat
   (stat (list-of statement?)))
  (if-stat
   (cond expression?)
   (stat1 statement?)
   (stat2 statement?))
  (cond-stat
   (conds (list-of expression?))
   (exps (list-of statement?)))
  (while-stat
   (cond expression?)
   (stat statement?))
  (var-stat
   (vars (list-of identifier?))
   (stat statement?))
  (nameless-var-stat
   (len integer?)
   (stat statement?)))

(define (map-of-two func lst1 lst2)
  (if (or (null? lst1) (null? lst2))
      '()
      (cons (func (car lst1) (car lst2))
            (map-of-two func (cdr lst1) (cdr lst2)))))

(define translation-of-exp
  (lambda (exp senv)
    (let ((trans-list (lambda (exp) (translation-of-exp exp senv))))
      (cases expression exp
        (const-exp (num) (const-exp num))
        (var-exp (var)
                 (let ((result (apply-senv senv var)))
                   (if (eqv? (caddr result) 'letrec)
                       (nameless-letrec-var-exp (car result) (cadr result))
                       (nameless-var-exp (car result) (cadr result)))))
        (add-exp (exp1 exp2)
                 (add-exp (translation-of-exp exp1 senv)
                          (translation-of-exp exp2 senv)))
        (sub-exp (exp1 exp2)
                 (sub-exp (translation-of-exp exp1 senv)
                          (translation-of-exp exp2 senv)))
        (mul-exp (exp1 exp2)
                 (mul-exp (translation-of-exp exp1 senv)
                          (translation-of-exp exp2 senv)))
        (div-exp (exp1 exp2)
                 (div-exp (translation-of-exp exp1 senv)
                          (translation-of-exp exp2 senv)))
        (minus-exp (exp)
                   (minus-exp (translation-of-exp exp senv)))
        (not-exp (exp)
                 (not-exp (translation-of-exp exp senv)))
        (and-exp (exp1 exp2)
                 (and-exp (translation-of-exp exp1 senv)
                          (translation-of-exp exp2 senv)))
        (or-exp (exp1 exp2)
                (or-exp (translation-of-exp exp1 senv)
                        (translation-of-exp exp2 senv)))
        (zero?-exp (exp)
                   (zero?-exp (translation-of-exp exp senv)))
        (equal?-exp (exp1 exp2)
                    (equal?-exp (translation-of-exp exp1 senv)
                                (translation-of-exp exp2 senv)))
        (less?-exp (exp1 exp2)
                   (less?-exp (translation-of-exp exp1 senv)
                              (translation-of-exp exp2 senv)))
        (greater?-exp (exp1 exp2)
                      (greater?-exp (translation-of-exp exp1 senv)
                                    (translation-of-exp exp2 senv)))
        (cons-exp (exp1 exp2)
                  (cons-exp (translation-of-exp exp1 senv)
                            (translation-of-exp exp2 senv)))
        (car-exp (exp)
                 (car-exp (translation-of-exp exp senv)))
        (cdr-exp (exp)
                 (cdr-exp (translation-of-exp exp senv)))
        (list-exp (exps)
                  (list-exp (map trans-list exps)))
        (null?-exp (exp)
                   (null?-exp (translation-of-exp exp senv)))
        (emptylist-exp () (emptylist-exp))
        (unpack-exp (vars exp1 exp2)
                    (nameless-unpack-exp (translation-of-exp exp1 senv)
                                         (translation-of-exp exp2 (extend-senv vars senv))))
        (let-exp (vars exps body)
                 (nameless-let-exp (map trans-list exps)
                                   (translation-of-exp body (extend-senv vars senv))))
        (let*-exp (vars exps body)
                  (if (null? vars)
                      (translation-of-exp body senv)
                      (translation-of-exp (let-exp (list (car vars))
                                               (list (car exps))
                                               (let*-exp (cdr vars)
                                                         (cdr exps)
                                                         body)) senv)))
        (proc-exp (vars body)
                  (nameless-proc-exp (translation-of-exp body (extend-senv vars senv))))
        (letproc-exp (names varss exps body)
                     (translation-of-exp (let-exp names
                                              (map-of-two (lambda (vars exp)
                                                            (proc-exp vars exp)) varss exps)
                                              body) senv))
        (letrec-exp (names varss exps body)
                    (let ((named-env (extend-senv-letrec names senv)))
                      (define (letrec-exp-rec varss exps)
                        (if (null? exps)
                            '()
                            (cons (translation-of-exp (car exps) (extend-senv (car varss) named-env))
                                  (letrec-exp-rec (cdr varss) (cdr exps)))))
                      (nameless-letrec-exp (letrec-exp-rec varss exps)
                                           (translation-of-exp body named-env))))
        (call-exp (rator rands)
                  (call-exp
                   (translation-of-exp rator senv)
                   (map trans-list rands)))
        (newref-exp (exp)
                    (newref-exp (translation-of-exp exp senv)))
        (deref-exp (var)
                   (nameless-deref-exp (translation-of-exp var senv)))
        (setref-exp (var exp)
                    (nameless-setref-exp (translation-of-exp var senv)
                                         (translation-of-exp exp senv)))
        (begin-exp (exp exps)
                   (begin-exp (translation-of-exp exp senv)
                              (map trans-list exps)))
        (assign-exp (var exp)
                    (let* ((result (apply-senv senv var))
                           (n (car result))
                           (pos (cadr result)))
                      (nameless-assign-exp n pos (translation-of-exp exp senv))))
        (setdynamic-exp (vars exps body)
                        (if (null? vars)
                            (translation-of-exp body senv)
                            (let* ((result (apply-senv senv (car vars)))
                                   (n (car result))
                                   (pos (cadr result)))
                              (nameless-setdynamic-exp n
                                                       pos
                                                       (translation-of-exp (car exps) senv)
                                                       (translation-of-exp
                                                        (setdynamic-exp (cdr vars)
                                                                        (cdr exps)
                                                                        body) senv)))))
        (else (error 'translation-of-exp "Invalid expression: " exp))))))

(define translation-of-stat
  (lambda (stat senv)
    (cases statement stat
      (assign-stat (var exp)
                   (let* ((result (apply-senv senv var))
                          (n (car result))
                          (pos (cadr result)))
                     (nameless-assign-stat n
                                           pos
                                           (translation-of-exp exp senv))))
      (print-stat (exp)
                  (print-stat (translation-of-exp exp senv)))
      (seq-stat (stats)
                (seq-stat (map (lambda (stat) (translation-of-stat stat senv)) stats)))
      (if-stat (exp stat1 stat2)
               (if-stat (translation-of-exp exp senv)
                        (translation-of-stat stat1 senv)
                        (translation-of-stat stat2 senv)))
      (cond-stat (conds stats)
                 (cond-stat (map (lambda (exp) (translation-of-exp exp senv)) conds)
                            (map (lambda (stat) (translation-of-stat stat senv)) stats)))
      (while-stat (exp stat)
                  (while-stat (translation-of-exp exp senv)
                              (translation-of-stat stat senv)))
      (var-stat (vars stat)
                (let ((next-senv (extend-senv vars senv)))
                  (nameless-var-stat (length vars)
                                     (translation-of-stat stat next-senv))))
      (else ('error 'translation-of-stat "Invalid statement: " stat)))))

(define (get-val value-of-result)
  (car value-of-result))

(define (get-store value-of-result)
  (cadr value-of-result))

(define (value-of-two-num extract construct op exp1 exp2 env store)
  (let* ((ret1 (value-of exp1 env store))
         (val1 (get-val ret1))
         (num1 (extract val1))
         (store1 (get-store ret1))
         (ret2 (value-of exp2 env store1))
         (val2 (get-val ret2))
         (num2 (extract val2))
         (store2 (get-store ret2)))
    (list (construct (op num1 num2)) store2)))

(define (value-of-list exps env store)
  (if (null? exps)
      (list '() store)
      (let* ((ret-head (value-of (car exps) env store))
             (val-head (get-val ret-head))
             (store-head (get-store ret-head))
             (ret-rest (value-of-list (cdr exps) env store-head))
             (val-rest (get-val ret-rest))
             (store-rest (get-store ret-rest)))
        (list (cons val-head val-rest) store-rest))))

(define (last l)
  (if (null? (cdr l))
      (car l)
      (last (cdr l))))

(define value-of
  (lambda (exp env store)
    (cases expression exp
      (const-exp (num) (list (num-val num) store))
      (add-exp (exp1 exp2) (value-of-two-num expval->num num-val + exp1 exp2 env store))
      (sub-exp (exp1 exp2) (value-of-two-num expval->num num-val - exp1 exp2 env store))
      (mul-exp (exp1 exp2) (value-of-two-num expval->num num-val * exp1 exp2 env store))
      (div-exp (exp1 exp2) (value-of-two-num expval->num num-val quotient exp1 exp2 env store))
      (minus-exp (exp)
                 (let* ((ret (value-of exp env store))
                        (val (get-val ret))
                        (num (expval->num val))
                        (next-store (get-store ret)))
                   (list (num-val (- num)) next-store)))
      (not-exp (exp)
               (let* ((ret (value-of exp env store))
                      (val (get-val ret))
                      (next-store (get-store ret)))
                 (list (bool-val (not (expval->bool val))) next-store)))
      (and-exp (exp1 exp2) (value-of-two-num expval->bool bool-val
                                             (lambda (x y) (and x y))
                                             exp1 exp2 env store))
      (or-exp (exp1 exp2) (value-of-two-num expval->bool bool-val
                                            (lambda (x y) (or x y))
                                            exp1 exp2 env store))
      (zero?-exp (exp)
                 (let* ((ret (value-of exp env store))
                        (val (get-val ret))
                        (num (expval->num val))
                        (next-store (get-store ret)))
                   (list (bool-val (zero? num)) next-store)))
      (equal?-exp (exp1 exp2) (value-of-two-num expval->num bool-val = exp1 exp2 env store))
      (less?-exp (exp1 exp2) (value-of-two-num expval->num bool-val < exp1 exp2 env store))
      (greater?-exp (exp1 exp2) (value-of-two-num expval->num bool-val > exp1 exp2 env store))
      (cons-exp (exp1 exp2)
                (let* ((ret1 (value-of exp1 env store))
                       (val1 (get-val ret1))
                       (store1 (get-store ret1))
                       (ret2 (value-of exp2 env store1))
                       (val2 (get-val ret2))
                       (store2 (get-store ret2)))
                  (list (list-val (cons val1
                                        (expval->list val2))) store2)))
      (car-exp (exp)
               (let* ((ret (value-of exp env store))
                      (val (get-val ret))
                      (next-store (get-store ret)))
                 (list (car (expval->list val)) next-store)))
      (cdr-exp (exp)
               (let* ((ret (value-of exp env store))
                      (val (get-val ret))
                      (next-store (get-store ret)))
                 (list (list-val (cdr (expval->list val))) next-store)))
      (list-exp (exps)
                (let* ((rets (value-of-list exps env store))
                       (vals (get-val rets))
                       (next-store (get-store rets)))
                  (list (list-val vals) next-store)))
      (null?-exp (exp)
                 (let* ((ret (value-of exp env store))
                        (val (get-val ret))
                        (lst (expval->list val))
                        (next-store (get-store ret)))
                   (list (bool-val (null? lst)) next-store)))
      (emptylist-exp ()
                     (list (list-val '()) store))
      (nameless-unpack-exp (exp1 exp2)
                (let* ((ret1 (value-of exp1 env store))
                       (val1 (get-val ret1))
                       (lst1 (expval->list val1))
                       (store1 (get-store ret1))
                       (ret-env (extend-env-ref lst1 env store1))
                       (next-env (get-val ret-env))
                       (next-store (get-store ret-env)))
                  (value-of exp2 next-env next-store)))
      (call-exp (rator rands)
                (let* ((ret-rator (value-of rator env store))
                       (val-rator (get-val ret-rator))
                       (store-rator (get-store ret-rator))
                       (proc (expval->proc val-rator))
                       (ret-rand (value-of-list rands env store-rator))
                       (vals (get-val ret-rand))
                       (store-end (get-store ret-rand)))
                  (apply-procedure proc vals store-end)))
      (nameless-var-exp (n pos)
                        (list (apply-env-ref env n pos store) store))
      (nameless-let-exp (exps body)
                        (let* ((rets (value-of-list exps env store))
                               (vals (get-val rets))
                               (store-exp (get-store rets))
                               (ret-env (extend-env-ref vals env store-exp))
                               (next-env (get-val ret-env))
                               (next-store (get-store ret-env)))
                          (value-of body next-env next-store)))
      (nameless-proc-exp (body)
                         (list (proc-val (procedure body env)) store))
      (nameless-letrec-exp (exps body)
                           (let* ((procs (map (lambda (exp) (proc-val (procedure exp env))) exps))
                                  (ret-env (extend-env-ref procs env store))
                                  (next-env (get-val ret-env))
                                  (next-store (get-store ret-env)))
                             (value-of body next-env next-store)))
      (nameless-letrec-var-exp (n pos)
                               (let ((proc-env (restore-env env n))
                                     (proc1 (expval->proc (apply-env-ref env n pos store))))
                                 (cases proc proc1
                                   (procedure (body saved-env)
                                              (list (proc-val (procedure body proc-env)) store)))))
      (newref-exp (exp)
                  (let* ((ret (value-of exp env store))
                         (val (get-val ret))
                         (next-store (get-store ret))
                         (ret-ref (newref next-store val))
                         (ref (get-val ret-ref))
                         (end-store (get-store ret-ref)))
                    (list (ref-val ref) end-store)))
      (nameless-deref-exp (var)
                          (let* ((ret (value-of var env store))
                                 (val (get-val ret))
                                 (next-store (get-store ret))
                                 (ref (expval->ref val)))
                            (list (deref store ref) next-store)))
      (nameless-setref-exp (var exp)
                           (let* ((ret1 (value-of var env store))
                                  (val1 (get-val ret1))
                                  (ref (expval->ref val1))
                                  (store1 (get-store ret1))
                                  (ret2 (value-of exp env store1))
                                  (val2 (get-val ret2))
                                  (store2 (get-store ret2))
                                  (next-store (setref store2 ref val2)))
                             (list val2 next-store)))
      (begin-exp (exp exps)
                 (let* ((rets (value-of-list (cons exp exps) env store))
                        (vals (get-val rets))
                        (next-store (get-store rets)))
                   (list (last vals) next-store)))
      (nameless-assign-exp (n pos exp)
                           (let* ((ref (apply-env env n pos))
                                  (ret (value-of exp env store))
                                  (val (get-val ret))
                                  (store-exp (get-store ret))
                                  (next-store (setref store-exp ref val)))
                             (list val next-store)))
      (nameless-setdynamic-exp (n pos exp body)
                               (let* ((ref (apply-env env n pos))
                                      (old-val (deref store ref))
                                      (ret (value-of exp env store))
                                      (val (get-val ret))
                                      (store-exp (get-store ret))
                                      (next-store (setref store-exp ref val))
                                      (ret-body (value-of body env next-store))
                                      (val-body (get-val ret-body))
                                      (store-body (get-store ret-body))
                                      (restored-store (setref store-body ref old-val)))
                                 (list val-body restored-store)))
      (else (error 'value-of "Invalid expression:" exp)))))

(define result-of
  (lambda (stat env store)
    (cases statement stat
      (nameless-assign-stat (n pos exp)
                            (let* ((ref (apply-env env n pos))
                                   (ret (value-of exp env store))
                                   (val (get-val ret))
                                   (store-exp (get-store ret))
                                   (next-store (setref store-exp ref val)))
                              (list val next-store)))
      (print-stat (exp)
                  (let* ((ret (value-of exp env store))
                         (val (get-val ret))
                         (next-store (get-store ret)))
                    (display (expval->val val))
                    (display "\n")
                    (list val next-store)))
      (seq-stat (stats)
                (define (seq-stat-rec stats env store)
                  (if (null? stats)
                      (list '() store)
                      (let* ((ret-head (result-of (car stats) env store))
                             (val-head (get-val ret-head))
                             (store-head (get-store ret-head))
                             (ret-rest (seq-stat-rec (cdr stats) env store-head))
                             (val-rest (get-val ret-rest))
                             (store-rest (get-store ret-rest)))
                        (list (cons val-head val-rest) store-rest))))
                (seq-stat-rec stats env store))
      (if-stat (exp stat1 stat2)
               (let* ((ret (value-of exp env store))
                      (val (get-val ret))
                      (next-store (get-store ret)))
                 (if (expval->bool val)
                     (result-of stat1 env next-store)
                     (result-of stat2 env next-store))))
      (cond-stat (conds stats)
                 (define (cond-exp-rec conds stats store)
                   (if (null? conds)
                       (error 'cond "No condition matched.")
                       (let* ((ret (value-of (car conds) env store))
                              (val (get-val ret))
                              (next-store (get-store ret)))
                         (if (expval->bool val)
                             (result-of (car stats) env next-store)
                             (cond-exp-rec (cdr conds) (cdr stats) next-store)))))
                 (cond-exp-rec conds stats store))
      (while-stat (exp stat)
                  (define (while-exp-rec store)
                    (let* ((ret (value-of exp env store))
                           (val (get-val ret))
                           (next-store (get-store ret)))
                      (if (expval->bool val)
                          (let ((stat-ret (result-of stat env next-store)))
                            (while-exp-rec (get-store stat-ret)))
                          (list val store))))
                  (while-exp-rec store))
      (nameless-var-stat (len stat)
                         (let* ((ret (extend-env-ref (build-list len (lambda (x) 0)) env store))
                                (next-env (get-val ret))
                                (next-store (get-store ret)))
                           (result-of stat next-env next-store)))
      (else (error 'value-of "Invalid statement:" exp)))))

; ===================
; Following are tests

(define (test prog expect)
  (display "Expect: \n")
  (if (list? expect)
      (map (lambda (x) (display x) (display "\n")) expect)
      (begin (display expect) (display  "\n")))
  (display "Actual: \n")
  (run prog)
  (display "\n"))

(define program-1 "var x, y; {
                       x = 3;
                       y = 4;
                       print +(x,y)
                   }")
(test program-1 7)

(define program-2 "var x, y, z; {
                       x = 3;
                       y = 4;
                       z = 0;
                       while not(zero?(x)) {
                           z = +(z, y);
                           x = -(x, 1)
                       };
                       print z
                   }")
(test program-2 12)

(define program-3 "var x; {
                       x = 3;
                       print x;
                       var x; {
                           x = 4;
                           print x};
                       print x
                   }")
(test program-3 '(3 4 3))

(define program-4 "var f, x; {
                       f = proc(x y) *(x, y);
                       x = 3;
                       print (f 4 x)
                   }")
(test program-4 12)
