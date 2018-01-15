(require eopl)

; BEGIN: Scanner
(define scanner-spec
  '((white-sp (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier (letter (arbno (or letter digit))) symbol)
    (number (digit (arbno digit)) number)
    (number ("-" digit (arbno digit)) number)))

(define scan&parse
  (sllgen:make-string-parser scanner-spec grammar))

(define-datatype program program?
  (a-program
   (exp expression?)))

(define (value-of-program pgm)
  (cases program pgm
    (a-program (exp)
               (value-of/k exp (empty-env) (end-cont)))))

(define (run prog)
  (initialize-store!)
  (value-of-program (scan&parse prog)))

; BEGIN: Value type
(define (identifier? x)
  (symbol? x))

(define (reference? v)
  (integer? v))

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
      (else (error 'num val)))))

(define expval->bool
  (lambda (val)
    (cases expval val
      (bool-val (bool) bool)
      (else (error 'bool val)))))

(define expval->proc
  (lambda (val)
    (cases expval val
      (proc-val (proc) proc)
      (else (error 'proc val)))))

(define expval->list
  (lambda (val)
    (cases expval val
      (list-val (lst) lst)
      (else (error 'list val)))))

(define expval->ref
  (lambda (val)
    (cases expval val
      (ref-val (ref) ref)
      (else (error 'ref val)))))

(define expval->val
  (lambda (val)
    (cases expval val
      (num-val (num) num)
      (bool-val (bool) bool)
      (list-val (lst)
                (map (lambda (val) (expval->val val)) lst))
      (proc-val (proc) proc)
      (ref-val (ref) (list 'ref ref)))))

; BEGIN: Store
(define (store? x)
  ((list-of expval?) x))

(define (empty-store)
  (make-vector 0))

(define the-store 'uninitialized)

(define (get-store) the-store)

(define (initialize-store!)
  (set! the-store (empty-store)))

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

; BEGIN: Environment
(define-datatype environment environment?
  (empty-env)
  (extend-env-ref
   (var identifier?)
   (ref reference?)
   (old-env environment?)))

(define (apply-env-ref env search-var)
  (cases environment env
    (empty-env ()
               (error 'apply-env-ref "Unbound variable." search-var))
    (extend-env-ref (var ref old-env)
                    (if (eqv? search-var var)
                        ref
                        (apply-env-ref old-env search-var)))))

(define (apply-env-val env var)
  (deref (apply-env-ref env var)))

(define (extend-env-val var val old-env)
  (extend-env-ref var (newref val) old-env))

(define (extend-env-vals vars vals env)
  (if (null? vars)
      env
      (extend-env-vals (cdr vars) (cdr vals) (extend-env-val (car vars) (car vals) env))))

(define (extend-env-rec names varss exps old-env)
  (let ((next-env (extend-env-vals names (build-list (length names) (lambda (x) 0)) old-env)))
    (define (extend-env-rec-sub names varss exps)
      (if (null? names)
          next-env
          (let* ((ref (apply-env-ref next-env (car names)))
                 (proc (procedure (car varss) (car exps) next-env)))
            (setref! ref (proc-val proc))
            (extend-env-rec-sub (cdr names) (cdr varss) (cdr exps)))))
    (extend-env-rec-sub names varss exps)))

; BEGIN: Grammar
(define grammar
  '((program (expression) a-program)
    (expression (number) const-exp)
    (expression (identifier) var-exp)
    (expression ("+" "(" expression "," expression ")") add-exp)
    (expression ("-" "(" expression "," expression ")") sub-exp)
    (expression ("*" "(" expression "," expression ")") mul-exp)
    (expression ("/" "(" expression "," expression ")") div-exp)
    (expression ("zero?" "(" expression ")") zero?-exp)
    (expression ("equal?" "(" expression "," expression ")") equal?-exp)
    (expression ("less?" "(" expression "," expression ")") less?-exp)
    (expression ("greater?" "(" expression "," expression ")") greater?-exp)
    (expression ("if" expression "then" expression "else" expression) if-exp)
    (expression ("cons" "(" expression "," expression ")") cons-exp)
    (expression ("car" "(" expression ")") car-exp)
    (expression ("cdr" "(" expression ")") cdr-exp)
    (expression ("list" "(" (arbno expression) ")") list-exp)
    (expression ("null?" "(" expression ")") null?-exp)
    (expression ("emptylist") emptylist-exp)
    (expression ("let" (arbno identifier "=" expression) "in" expression) let-exp)
    (expression ("letrec" (arbno identifier "(" (arbno identifier) ")" "=" expression)
                 "in" expression) letrec-exp)
    (expression ("ref" identifier) ref-exp)
    (expression ("newref" "(" expression ")") newref-exp)
    (expression ("deref" "(" expression ")") deref-exp)
    (expression ("setref" "(" expression "," expression ")") setref-exp)
    (expression ("set" identifier "=" expression) set-exp)
    (expression ("begin" (separated-list expression ";") "end") begin-exp) 
    (expression ("proc" "(" (arbno identifier) ")" expression) proc-exp)
    (expression ("(" expression (arbno expression) ")") call-exp)))

; BEGIN: Expression
(define-datatype expression expression?
  (const-exp (num number?))
  (var-exp (var identifier?))
  (add-exp (exp1 expression?)
           (exp2 expression?))
  (sub-exp (exp1 expression?)
           (exp2 expression?))
  (mul-exp (exp1 expression?)
           (exp2 expression?))
  (div-exp (exp1 expression?)
           (exp2 expression?))
  (zero?-exp (exp expression?))
  (equal?-exp (exp1 expression?)
              (exp2 expression?))
  (less?-exp (exp1 expression?)
             (exp2 expression?))
  (greater?-exp (exp1 expression?)
                (exp2 expression?))
  (if-exp (cond expression?)
          (exp-t expression?)
          (exp-f expression?))
  (cons-exp (exp1 expression?)
            (exp2 expression?))
  (car-exp (exp expression?))
  (cdr-exp (exp expression?))
  (list-exp (exps (list-of expression?)))
  (null?-exp (exp expression?))
  (emptylist-exp)
  (let-exp (vars (list-of identifier?))
           (exps (list-of expression?))
           (body expression?))
  (letrec-exp (names (list-of identifier?))
              (varss (list-of (list-of identifier?)))
              (exps (list-of expression?))
              (body expression?))
  (ref-exp (var identifier?))
  (newref-exp (exp expression?))
  (deref-exp (var expression?))
  (setref-exp (var expression?)
              (exp expression?))
  (set-exp (var identifier?)
           (exp expression?))
  (begin-exp (exps (list-of expression?)))
  (proc-exp (vars (list-of identifier?))
            (body expression?))
  (call-exp (rator expression?)
            (rands (list-of expression?))))

; BEGIN: Continuation
(define-datatype continuation continuation?
  (dual-1-cont (op procedure?)
               (exp2 expression?)
               (env environment?)
               (saved-cont continuation?))
  (dual-2-cont (op procedure?)
               (val1 expval?)
               (saved-cont continuation?))
  (compare-1-cont (op procedure?)
                  (exp2 expression?)
                  (env environment?)
                  (saved-cont continuation?))
  (compare-2-cont (op procedure?)
                  (val1 expval?)
                  (saved-cont continuation?))
  (zero?-cont (saved-cont continuation?))
  (if-cont (exp-t expression?)
           (exp-f expression?)
           (env environment?)
           (saved-cont continuation?))
  (cons-1-cont (exp2 expression?)
               (env environment?)
               (saved-cont continuation?))
  (cons-2-cont (val1 expval?)
               (saved-cont continuation?))
  (car-cont (saved-cont continuation?))
  (cdr-cont (saved-cont continuation?))
  (list-cont (exps (list-of expression?))
             (vals (list-of expval?))
             (env environment?)
             (saved-cont continuation?))
  (null?-cont (saved-cont continuation?))
  (let-cont (vars (list-of identifier?))
            (exps (list-of expression?))
            (vals (list-of expval?))
            (body expression?)
            (env environment?)
            (saved-cont continuation?))
  (newref-cont (saved-cont continuation?))
  (deref-cont (saved-cont continuation?))
  (setref-1-cont (exp expression?)
                 (env environment?)
                 (saved-cont continuation?))
  (setref-2-cont (var expval?)
                 (saved-cont continuation?))
  (set-cont (ref reference?)
            (saved-cont continuation?))
  (begin-cont (exps (list-of expression?))
              (env environment?)
              (saved-cont continuation?))
  (rator-cont (rands (list-of expression?))
              (env environment?)
              (saved-cont continuation?))
  (rands-cont (rator expval?)
              (rands (list-of expression?))
              (vals (list-of expval?))
              (env environment?)
              (saved-cont continuation?))
  (end-cont))

(define (apply-cont cont val)
  (display cont)
  (display "\n")
  (display val)
  (display "\n\n")
  (cases continuation cont
    (dual-1-cont (op exp2 env saved-cont)
                (let ((val1 val))
                  (value-of/k exp2 env (dual-2-cont op val1 saved-cont))))
    (dual-2-cont (op val1 saved-cont)
                (let ((val2 val))
                  (let ((num1 (expval->num val1))
                        (num2 (expval->num val2)))
                    (apply-cont saved-cont (num-val (op num1 num2))))))
    (compare-1-cont (op exp2 env saved-cont)
                    (let ((val1 val))
                      (value-of/k exp2 env (compare-2-cont op val1 saved-cont))))
    (compare-2-cont (op val1 saved-cont)
                    (let ((val2 val))
                      (let ((num1 (expval->num val1))
                            (num2 (expval->num val2)))
                        (apply-cont saved-cont (bool-val (op num1 num2))))))
    (zero?-cont (saved-cont)
                (apply-cont saved-cont (bool-val (zero? (expval->num val)))))
    (if-cont (exp-t exp-f env saved-cont)
             (if (expval->bool val)
                 (value-of/k exp-t env saved-cont)
                 (value-of/k exp-f env saved-cont)))
    (cons-1-cont (exp2 env saved-cont)
                 (let ((val1 val))
                   (value-of/k exp2 env (cons-2-cont val1 saved-cont))))
    (cons-2-cont (val1 saved-cont)
                 (let ((val2 val))
                   (apply-cont saved-cont (list-val (cons val1 (expval->list val2))))))
    (car-cont (saved-cont)
              (apply-cont saved-cont (car (expval->list val))))
    (cdr-cont (saved-cont)
              (apply-cont saved-cont (list-val (cdr (expval->list val)))))
    (list-cont (exps vals env saved-cont)
               (let ((next-val (cons val vals)))
                 (if (null? exps)
                     (apply-cont saved-cont (list-val (reverse next-val)))
                     (value-of/k (car exps) env (list-cont (cdr exps) next-val env saved-cont)))))
    (null?-cont (saved-cont)
                (apply-cont saved-cont (bool-val (null? (expval->list val)))))
    (let-cont (vars exps vals body env saved-cont)
              (let ((next-val (cons val vals)))
                (if (null? exps)
                    (value-of/k body (extend-env-vals vars (reverse next-val) env) saved-cont)
                    (value-of/k (car exps) env (let-cont vars (cdr exps)
                                                         next-val
                                                         body env saved-cont)))))
    (newref-cont (saved-cont)
                 (let ((ref (newref val)))
                   (apply-cont saved-cont (ref-val ref))))
    (deref-cont (saved-cont)
                (let ((real-val (deref (expval->ref val))))
                  (apply-cont saved-cont real-val)))
    (set-cont (ref saved-cont)
              (setref! ref val)
              (apply-cont saved-cont val))
    (setref-1-cont (exp env saved-cont)
                   (value-of/k exp env (setref-2-cont val saved-cont)))
    (setref-2-cont (var saved-cont)
                   (begin (setref! (expval->ref var) val)
                          (apply-cont saved-cont val)))
    (begin-cont (exps env saved-cont)
                (if (null? exps)
                    val
                    (value-of/k (car exps) env (begin-cont (cdr exps) env saved-cont))))
    (rator-cont (rands env saved-cont)
                (if (null? rands)
                    (apply-procedure/k (expval->proc val) '() saved-cont)
                    (value-of/k (car rands) env (rands-cont val (cdr rands) '() env saved-cont))))
    (rands-cont (rator rands vals env saved-cont)
                (let ((next-val (cons val vals)))
                  (if (null? rands)
                      (apply-procedure/k (expval->proc rator) (reverse next-val) saved-cont)
                      (value-of/k (car rands) env (rands-cont rator (cdr rands)
                                                              next-val env saved-cont)))))
    (end-cont ()
              val)))

; BEGIN: Procedure
(define-datatype proc proc?
  (procedure (vars (list-of identifier?))
             (body expression?)
             (saved-env environment?)))

(define (apply-procedure/k proc1 vals cont)
  (cases proc proc1
    (procedure (vars body saved-env)
               (value-of/k body (extend-env-vals vars vals saved-env) cont))))

; BEGIN: Evaluation
(define (value-of/k exp env cont)
  (cases expression exp
    (const-exp (num)
               (apply-cont cont (num-val num)))
    (var-exp (var)
             (apply-cont cont (apply-env-val env var)))
    (add-exp (exp1 exp2)
             (value-of/k exp1 env (dual-1-cont + exp2 env cont)))
    (sub-exp (exp1 exp2)
             (value-of/k exp1 env (dual-1-cont - exp2 env cont)))
    (mul-exp (exp1 exp2)
             (value-of/k exp1 env (dual-1-cont * exp2 env cont)))
    (div-exp (exp1 exp2)
             (value-of/k exp1 env (dual-1-cont quotient exp2 env cont)))
    (zero?-exp (exp)
               (value-of/k exp env (zero?-cont cont)))
    (equal?-exp (exp1 exp2)
                (value-of/k exp1 env (compare-1-cont = exp2 env cont)))
    (less?-exp (exp1 exp2)
               (value-of/k exp1 env (compare-1-cont < exp2 env  cont)))
    (greater?-exp (exp1 exp2)
                  (value-of/k exp1 env (compare-1-cont > exp2 env  cont)))
    (if-exp (cond exp-t exp-f)
            (value-of/k cond env (if-cont exp-t exp-f env cont)))
    (cons-exp (exp1 exp2)
              (value-of/k exp1 env (cons-1-cont exp2 env cont)))
    (car-exp (exp)
             (value-of/k exp env (car-cont cont)))
    (cdr-exp (exp)
             (value-of/k exp env (cdr-cont cont)))
    (list-exp (exps)
              (if (null? exps)
                  (apply-cont cont (list-val '()))
                  (value-of/k (car exps) env (list-cont (cdr exps) '() env cont))))
    (null?-exp (exp)
               (value-of/k exp env (null?-cont cont)))
    (emptylist-exp ()
                   (apply-cont cont (list-val '())))
    (let-exp (vars exps body)
             (if (null? vars)
                 (value-of/k body env cont)
                 (value-of/k (car exps) env (let-cont vars (cdr exps) '() body env cont))))
    (letrec-exp (names varss exps body)
                (value-of/k body (extend-env-rec names varss exps env) cont))
    (ref-exp (var)
             (apply-cont cont (ref-val (apply-env-ref env var))))
    (newref-exp (exp)
                (value-of/k exp env (newref-cont cont)))
    (deref-exp (var)
               (value-of/k var env (deref-cont cont)))
    (setref-exp (var exp)
                (value-of/k var env (setref-1-cont exp env cont)))
    (set-exp (var exp)
             (value-of/k exp env (set-cont (apply-env-ref env var) cont)))
    (begin-exp (exps)
               (apply-cont (begin-cont exps env cont) (num-val 0)))
    (proc-exp (vars body)
              (apply-cont cont (proc-val (procedure vars body env))))
    (call-exp (rator rands)
              (value-of/k rator env (rator-cont rands env cont)))))

; BEGIN: Tests
(define (equal?! prog expect)
  (display "Expect: ")
  (display expect)
  (display "\nActual: ")
  (let ((actual (expval->val (run prog))))
    (display actual)
    (display "\n")
    (if (equal? actual expect)
        (display "\n")
        (display "Wrong Answer!!\n\n"))))

(define program-const "1")
(equal?! program-const 1)

(define program-diff "-(1, -2)")
(equal?! program-diff 3)

(define program-zero "zero?(0)")
(equal?! program-zero #t)

(define program-let "let x = 1 y = 2 in +(1, 2)")
(equal?! program-let 3)

(define program-proc "
let f = proc(a b) -(a, b)
in (f 4 2)")
(equal?! program-proc 2)

(define program-letrec-1 "
letrec gcd(a b) = if equal?(a, b)
                  then a
                  else if less?(a, b)
                       then (gcd a -(b, a))
                       else (gcd -(a, b) b)
in (gcd 24 60)")
(equal?! program-letrec-1 12)

(define program-letrec-2 "
letrec odd(a) = if zero?(a) then 0 else (even -(a, 1))
       even(a) = if zero?(a) then 1 else (odd -(a, 1))
in (odd 11)")
(equal?! program-letrec-2 1)

(define program-list "list(1 2 3)")
(equal?! program-list '(1 2 3))

(define program-car "car(list(1 2 3))")
(equal?! program-car 1)

(define program-cdr "cdr(list(1 2 3))")
(equal?! program-cdr '(2 3))

(define program-null "null?(emptylist)")
(equal?! program-null #t)

(define program-cons "cons(1, list(2 3))")
(equal?! program-cons '(1 2 3))

(define program-deref "deref(newref(1))")
(equal?! program-deref 1)

(define program-setref "let a = newref(1) in let b = setref(a, 2) in list(deref(a) b)")
(equal?! program-setref '(2 2))

(define program-ref "let a = 1 in let b = ref a in let c = setref(b, 2) in a")
(equal?! program-ref 2)

(define program-set "let a = 1 in let b = set a = 2 in list(a b)")
(equal?! program-set '(2 2))

(define program-begin "let a = 1 in begin set a = 2; +(a, 1) end")
(equal?! program-begin 3)

(define program-fact "
letrec fact(n) = if zero?(n) then 1 else *(n, (fact -(n, 1)))
in (fact 10)")
(equal?! program-fact 3628800)

(define program-fact-iter "
letrec fact(n val) = if zero?(n) then val else (fact -(n, 1) *(val, n))
in (fact 10 1)")
(equal?! program-fact-iter 3628800)
