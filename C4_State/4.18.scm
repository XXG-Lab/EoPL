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
    (car
     (value-of-program
      (translation-of-program
       (a-program
        (scan&parse string)))))))
  
(define (value-of-program pgm)
  (cases program pgm
    (a-program (exp)
               (value-of exp (empty-env) (empty-store)))))

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
   (exp expression?))
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
  (call-exp
   (rator expression?)
   (rands (list-of expression?))))

(define (map-of-two func lst1 lst2)
  (if (or (null? lst1) (null? lst2))
      '()
      (cons (func (car lst1) (car lst2))
            (map-of-two func (cdr lst1) (cdr lst2)))))

(define translation-of
  (lambda (exp senv)
    (let ((trans-list (lambda (exp) (translation-of exp senv))))
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
        (minus-exp (exp)
                   (minus-exp (translation-of exp senv)))
        (zero?-exp (exp)
                   (zero?-exp (translation-of exp senv)))
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
                  (cond-exp (map trans-list conds)
                            (map trans-list exps)))
        (cons-exp (exp1 exp2)
                  (cons-exp (translation-of exp1 senv)
                            (translation-of exp2 senv)))
        (car-exp (exp)
                 (car-exp (translation-of exp senv)))
        (cdr-exp (exp)
                 (cdr-exp (translation-of exp senv)))
        (list-exp (exps)
                  (list-exp (map trans-list exps)))
        (null?-exp (exp)
                   (null?-exp (translation-of exp senv)))
        (emptylist-exp () (emptylist-exp))
        (unpack-exp (vars exp1 exp2)
                    (nameless-unpack-exp (translation-of exp1 senv)
                                         (translation-of exp2 (extend-senv vars senv))))
        (let-exp (vars exps body)
                 (nameless-let-exp (map trans-list exps)
                                   (translation-of body (extend-senv vars senv))))
        (let*-exp (vars exps body)
                  (if (null? vars)
                      (translation-of body senv)
                      (translation-of (let-exp (list (car vars))
                                               (list (car exps))
                                               (let*-exp (cdr vars)
                                                         (cdr exps)
                                                         body)) senv)))
        (proc-exp (vars body)
                  (nameless-proc-exp (translation-of body (extend-senv vars senv))))
        (letproc-exp (names varss exps body)
                     (translation-of (let-exp names
                                              (map-of-two (lambda (vars exp)
                                                            (proc-exp vars exp)) varss exps)
                                              body) senv))
        (letrec-exp (names varss exps body)
                    (let ((named-env (extend-senv-letrec names senv)))
                      (define (letrec-exp-rec varss exps)
                        (if (null? exps)
                            '()
                            (cons (translation-of (car exps) (extend-senv (car varss) named-env))
                                  (letrec-exp-rec (cdr varss) (cdr exps)))))
                      (nameless-letrec-exp (letrec-exp-rec varss exps)
                                           (translation-of body named-env))))
        (call-exp (rator rands)
                  (call-exp
                   (translation-of rator senv)
                   (map trans-list rands)))
        (newref-exp (exp)
                    (newref-exp (translation-of exp senv)))
        (deref-exp (var)
                   (nameless-deref-exp (translation-of var senv)))
        (setref-exp (var exp)
                    (nameless-setref-exp (translation-of var senv)
                                         (translation-of exp senv)))
        (begin-exp (exp exps)
                   (begin-exp (translation-of exp senv)
                              (map trans-list exps)))
        (assign-exp (var exp)
                    (let* ((result (apply-senv senv var))
                           (n (car result))
                           (pos (cadr result)))
                      (nameless-assign-exp n pos (translation-of exp senv))))
        (else (error 'translation-of "Invalid expression: " exp))))))

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
      (zero?-exp (exp)
                 (let* ((ret (value-of exp env store))
                        (val (get-val ret))
                        (num (expval->num val))
                        (next-store (get-store ret)))
                   (list (bool-val (zero? num)) next-store)))
      (equal?-exp (exp1 exp2) (value-of-two-num expval->num bool-val = exp1 exp2 env store))
      (less?-exp (exp1 exp2) (value-of-two-num expval->num bool-val < exp1 exp2 env store))
      (greater?-exp (exp1 exp2) (value-of-two-num expval->num bool-val > exp1 exp2 env store))
      (if-exp (exp1 exp2 exp3)
              (let* ((ret1 (value-of exp1 env store))
                     (val1 (get-val ret1))
                     (store1 (get-store ret1)))
                (if (expval->bool val1)
                    (value-of exp2 env store1)
                    (value-of exp3 env store1))))
      (cond-exp (conds exps)
                (define (cond-exp-rec conds exps store)
                  (if (null? conds)
                      (error 'cond "No condition matched.")
                      (let* ((ret1 (value-of (car conds) env store))
                             (val1 (get-val ret1))
                             (store1 (get-store ret1)))
                        (if (expval->bool val1)
                            (value-of (car exps) env store1)
                            (cond-exp-rec (cdr conds) (cdr exps) store1)))))
                (cond-exp-rec conds exps store))
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
      (else (error 'value-of "Invalid expression: " exp)))))

; ===================
; Following are tests

(define (equal?! prog expect)
  (let ((actual (expval->val (run prog))))
    (if (equal? actual expect)
        (begin
          ;(display prog)
          ;(display "\n")
          (display "[Passed] Result: ")
          (display actual)
          (display "\n"))
        (begin
          (display prog)
          (display "\nActual: ")
          (display actual)
          (display " Expect: ")
          (display expect)
          (display "\n")))
    (display "\n")))

(define program-op "*(-(2, 1), +(/(10, 5), 1))")
(equal?! program-op 3)

(define program-minus "minus(4)")
(equal?! program-minus -4)

(define program-zero?-t "zero?(0)")
(equal?! program-zero?-t #t)

(define program-zero?-f "zero?(1)")
(equal?! program-zero?-f #f)

(define program-equal?-t "equal?(0, 0)")
(equal?! program-equal?-t #t)

(define program-equal?-f "equal?(0, 1)")
(equal?! program-equal?-f #f)

(define program-less?-t "less?(0, 1)")
(equal?! program-less?-t #t)

(define program-less?-f "less?(0, 0)")
(equal?! program-less?-f #f)

(define program-greater?-t "greater?(1, 0)")
(equal?! program-greater?-t #t)

(define program-greater?-f "greater?(0, 0)")
(equal?! program-greater?-f #f)

(define program-if-0 "if zero?(0) then 0 else 1")
(equal?! program-if-0 0)

(define program-if-1 "if zero?(1) then 0 else 1")
(equal?! program-if-1 1)

(define program-cond "cond zero?(1) ==> 0 zero?(0) ==> 1 end")
(equal?! program-cond 1)

(define program-cons "cons(1, emptylist)")
(equal?! program-cons '(1))

(define program-list "list(1 2 3)")
(equal?! program-list '(1 2 3))

(define program-car "car(list(1 2 3))")
(equal?! program-car 1)

(define program-cdr "cdr(list(1 2 3))")
(equal?! program-cdr '(2 3))

(define program-null?-t "null?(emptylist)")
(equal?! program-null?-t #t)

(define program-null?-f "null?(list(1 2 3))")
(equal?! program-null?-f #f)

(define program-unpack "unpack a b c = list(1 2 3) in list(c b a)")
(equal?! program-unpack '(3 2 1))

(define program-let "let a = 1 b = 2 in list(a b)")
(equal?! program-let '(1 2))

(define program-let* "let* a = 1 b = a in list(a b)")
(equal?! program-let* '(1 1))

(define program-proc "let f = proc(a b) -(a, b) in (f 3 1)")
(equal?! program-proc 2)

(define program-letproc "letproc f(n) = -(n, 1)
                                 g(n) = +(n, 1)
                         in list((f 0) (g 0))")
(equal?! program-letproc '(-1 1))

(define program-letrec-s "letrec fact(n) = if zero?(n) then 1 else *(n, (fact -(n, 1)))
                          in (fact 10)")
(equal?! program-letrec-s 3628800)

(define program-letrec-m "letrec odd(n dummy) = if zero?(n) then 0 else (even -(n, 1) dummy)
                                 even(n dummy) = if zero?(n) then 1 else (odd -(n, 1) dummy)
                          in (odd 13 100)")
(equal?! program-letrec-m 1)

(define program-state-list "let a = newref(0)
                            in list(list(setref(a, 1) setref(a, 2) setref(a, 3)) setref(a, 4))")
(equal?! program-state-list '((1 2 3) 4))

(define program-state-op "let a = newref(0)
                          in -(-(setref(a, 10), setref(a, 4)), setref(a, 3))")
(equal?! program-state-op 3)

(define program-state-minus "let a = newref(0)
                             in begin
                                  minus(setref(a, 1));
                                  minus(deref(a))
                                end")
(equal?! program-state-minus -1)

(define program-state-zero? "let a = newref(0)
                             in list(zero?(deref(a)) zero?(setref(a, 1)) zero?(deref(a)))")
(equal?! program-state-zero? '(#t #f #f))

(define program-state-equal? "let a = newref(0) b = newref(0)
                              in list(equal?(deref(a), deref(b))
                                      equal?(setref(a, 1), deref(b))
                                      equal?(deref(a), setref(b, 1)))")
(equal?! program-state-equal? '(#t #f #t))

(define program-state-less? "let a = newref(0) b = newref(1)
                             in list(less?(deref(a), deref(b))
                                     less?(setref(a, 1), deref(b))
                                     less?(deref(a), setref(b, 2)))")
(equal?! program-state-less? '(#t #f #t))

(define program-state-greater? "let a = newref(2) b = newref(1)
                                in list(greater?(deref(a), deref(b))
                                        greater?(setref(a, 1), deref(b))
                                        greater?(deref(a), setref(b, 0)))")
(equal?! program-state-greater? '(#t #f #t))

(define program-state-if-t "let a = newref(1)
                            in list(if zero?(setref(a, 0))
                                    then setref(a, 2)
                                    else setref(a, 3) deref(a))")
(equal?! program-state-if-t '(2 2))

(define program-state-if-f "let a = newref(0)
                            in list(if zero?(setref(a, 1))
                                    then setref(a, 2)
                                    else setref(a, 3) deref(a))")
(equal?! program-state-if-f '(3 3))

(define program-state-cond "let a = newref(0)
                            in list(cond
                                      zero?(setref(a, 1)) ==> setref(a, 0)
                                      zero?(deref(a)) ==> setref(a, 2)
                                      zero?(0) ==> setref(a, 3)
                                    end deref(a))")
(equal?! program-state-cond '(3 3))

(define program-state-cons "let a = newref(0)
                            in list(cons(setref(a, 1), emptylist) deref(a))")
(equal?! program-state-cons '((1) 1))

(define program-state-car "let a = newref(0)
                           in list(car(list(setref(a, 3))) deref(a))")
(equal?! program-state-car '(3 3))

(define program-state-cdr "let a = newref(0)
                           in list(cdr(list(setref(a, 3) setref(a, 2))) deref(a))")
(equal?! program-state-cdr '((2) 2))

(define program-state-null? "let a = newref(0)
                             in list(null?(list(setref(a, 1))) deref(a))")
(equal?! program-state-null? '(#f 1))

(define program-state-unpack "let a = newref(0)
                              in unpack b c d = list(setref(a, 1) setref(a, 2) setref(a, 3))
                                 in list(b c d deref(a))")
(equal?! program-state-unpack '(1 2 3 3))

(define program-state-let "let a = newref(0)
                           in let b = setref(a, 1)
                                  c = deref(a)
                              in list(b c deref(a))")
(equal?! program-state-let '(1 1 1))

(define program-state-let* "let a = newref(0)
                            in let* b = setref(a, 1)
                                    c = +(b, setref(a, 2))
                               in list(deref(a) b c)")
(equal?! program-state-let* '(2 1 3))


(define program-1
  "let x = newref(0)
   in letrec even(dummy) = if zero?(deref(x))
                           then 1
                           else begin
                                  setref(x, -(deref(x),1));
                                  (odd 888)
                                end
             odd(dummy) = if zero?(deref(x))
                          then 0
                          else begin
                                 setref(x, -(deref(x),1));
                                 (even 888)
                               end
      in begin 
           setref(x,13); 
           (odd 888) 
         end")
(equal?! program-1 1)

(define program-2
  "let g = let counter = newref(0)
           in proc (dummy)
                begin
                  setref(counter, -(deref(counter), minus(1)));
                  deref(counter)
                end
   in let a = (g 11)
      in let b = (g 11)
         in -(a,b)")
(equal?! program-2 -1)

(define program-3
  "let x = newref(newref(0))
   in begin
        setref(deref(x), 11);
        deref(deref(x))
      end")
(equal?! program-3 11)

(define program-4
  "let x = newref(0)
   in let y = list(setref(x, 1) deref(x))
      in car(cdr(y))")
(equal?! program-4 1)

(define program-5
  "let x = 0
   in letrec even(dummy) = if zero?(x)
                           then 1
                           else begin
                                  set x = -(x,1);
                                  (odd 888)
                                end
             odd(dummy) = if zero?(x)
                          then 0
                          else begin
                                 set x = -(x,1);
                                 (even 888)
                               end
     in begin 
          set x = 13; 
          (odd 888) 
        end")
(equal?! program-5 1)

(define program-6
  "let g = let count = 0
           in proc (dummy) 
                begin
                  set count = -(count,minus(1));
                  count
                end
   in let a = (g 11)
      in let b = (g 11)
         in -(a,b)")
(equal?! program-6 -1)

(define program-7
  "let times4 = 0
   in begin
        set times4 = proc (x) if zero?(x)
                              then 0
                              else -((times4 -(x,1)), minus(4));
        (times4 3)
      end")
(equal?! program-7 12)

;```
;                    (value-of var1 exp1 ρ σ0) = (val1, σ1)
;           (value-of var2 exp2 [var1=l1]ρ [l1=val1]σ1) = (val2, σ2)
;     (value-of var3 exp3 [var1=l1, var2=l2]ρ [l2=val2]σ2) = (val3, σ3)
;(value-of varn expn [var1=l1, ..., var(n-1)=l(n-1)]ρ [l(n-1)=val(n-1)]σ(n-1)) = (valn, σn)
;------------------------------------------------------------------------------------------
;     (value-of (letrec vars exps body) ρ σ0) = (value-of body [varn=ln]ρ [ln=valn]σn)
;```
