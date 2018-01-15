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

(define-datatype proc proc?
  (procedure (var identifier?)
             (body expression?)
             (saved-env environment?)))

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

(define (extend-env-rec name var exp old-env)
  (let* ((next-env (extend-env-val name 0 old-env))
         (ref (apply-env-ref next-env name))
         (proc (procedure var exp next-env)))
    (begin
      (setref! ref (proc-val proc))
      next-env)))

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
    (expression ("if" expression "then" expression "else" expression) if-exp)
    (expression ("cons" "(" expression "," expression ")") cons-exp)
    (expression ("car" "(" expression ")") car-exp)
    (expression ("cdr" "(" expression ")") cdr-exp)
    (expression ("list" "(" (arbno expression) ")") list-exp)
    (expression ("null?" "(" expression ")") null?-exp)
    (expression ("emptylist") emptylist-exp)
    (expression ("let" (arbno identifier "=" expression) "in" expression) let-exp)
    (expression ("letrec" identifier "(" identifier ")" "=" expression "in" expression) letrec-exp)
    (expression ("proc" "(" identifier ")" expression) proc-exp)
    (expression ("(" expression expression ")") call-exp)))

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
  (letrec-exp (name identifier?)
              (var identifier?)
              (exp expression?)
              (body expression?))
  (proc-exp (vars identifier?)
            (body expression?))
  (call-exp (rator expression?)
            (rand expression?)))

; BEGIN: Continuation
(define-datatype continuation continuation?
  (dual-1-cont (op procedure?)
               (exp2 expression?)
               (env environment?)
               (saved-cont continuation?))
  (dual-2-cont (op procedure?)
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
  (rator-cont (rand expression?)
              (env environment?)
              (saved-cont continuation?))
  (rand-cont (val1 expval?)
             (saved-cont continuation?))
  (end-cont))

(define (apply-cont cont val)
  (cases continuation cont
    (dual-1-cont (op exp2 env saved-cont)
                (let ((val1 val))
                  (value-of/k exp2 env (dual-2-cont op val1 saved-cont))))
    (dual-2-cont (op val1 saved-cont)
                (let ((val2 val))
                  (let ((num1 (expval->num val1))
                        (num2 (expval->num val2)))
                    (apply-cont saved-cont (num-val (op num1 num2))))))
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
               (if (null? exps)
                   (apply-cont saved-cont (list-val (reverse (cons val vals))))
                   (value-of/k (car exps) env (list-cont (cdr exps) (cons val vals) env saved-cont))))
    (null?-cont (saved-cont)
                (apply-cont saved-cont (bool-val (null? (expval->list val)))))
    (let-cont (vars exps vals body env saved-cont)
              (if (null? exps)
                  (value-of/k body (extend-env-vals vars (reverse (cons val vals)) env) saved-cont)
                  (value-of/k (car exps) env (let-cont vars (cdr exps)
                                                       (cons val vals)
                                                       body env saved-cont))))
    (rator-cont (rand env saved-cont)
                (value-of/k rand env (rand-cont val saved-cont)))
    (rand-cont (rator-val saved-cont)
               (let ((proc1 (expval->proc rator-val)))
                 (apply-procedure/k proc1 val saved-cont)))
    (end-cont ()
              val)))

; BEGIN: Evaluation
(define (apply-procedure/k proc1 val cont)
  (cases proc proc1
    (procedure (var body saved-env)
               (value-of/k body (extend-env-val var val saved-env) cont))))

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
    (letrec-exp (name var exp body)
                (value-of/k body (extend-env-rec name var exp env) cont))
    (proc-exp (var body)
              (apply-cont cont (proc-val (procedure var body env))))
    (call-exp (rator rand)
              (value-of/k rator env (rator-cont rand env cont)))))

; BEGIN: Tests
(define (equal?! prog expect)
  (display "Expect: ")
  (display expect)
  (display "\nActual: ")
  (let ((actual (expval->val (run prog))))
    (display actual)
    (display "\n")
    (if (equal? actual expect)
        (display "")
        (display "Wrong Answer!!"))
    (display "\n\n")))

(define program-const "1")
(equal?! program-const 1)

(define program-diff "-(1, -2)")
(equal?! program-diff 3)

(define program-zero "zero?(0)")
(equal?! program-zero #t)

(define program-let "let x = 1 y = 2 in +(1, 2)")
(equal?! program-let 3)

(define program-proc "
let f = proc(n) -(n, 1)
in (f 3)")
(equal?! program-proc 2)

(define program-letrec "
letrec f(n) = if zero?(n)
              then 0
              else -((f -(n, 1)), -2)
in (f 4)")
(equal?! program-letrec 8)

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
