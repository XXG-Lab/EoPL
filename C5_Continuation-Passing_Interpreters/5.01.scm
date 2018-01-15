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
    (expression ("-" "(" expression "," expression ")") diff-exp)
    (expression ("zero?" "(" expression ")") zero?-exp)
    (expression ("if" expression "then" expression "else" expression) if-exp)
    (expression ("let" identifier "=" expression "in" expression) let-exp)
    (expression ("letrec" identifier "(" identifier ")" "=" expression "in" expression) letrec-exp)
    (expression ("proc" "(" identifier ")" expression) proc-exp)
    (expression ("(" expression expression ")") call-exp)))

; BEGIN: Expression
(define-datatype expression expression?
  (const-exp (num number?))
  (var-exp (var identifier?))
  (diff-exp (exp1 expression?)
            (exp2 expression?))
  (zero?-exp (exp expression?))
  (if-exp (cond expression?)
          (exp-t expression?)
          (exp-f expression?))
  (let-exp (var identifier?)
           (exp expression?)
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
(define end-cont
  (lambda ()
    (lambda (val)
      val)))

(define zero?-cont
  (lambda (cont)
    (lambda (val)
      (apply-cont cont
                  (bool-val
                   (zero? (expval->num val)))))))

(define let-cont
  (lambda (var body env cont)
    (lambda (val)
      (value-of/k body (extend-env-val var val env) cont))))

(define if-cont
  (lambda (exp2 exp3 env cont)
    (lambda (val)
      (if (expval->bool val)
          (value-of/k exp2 env cont)
          (value-of/k exp3 env cont)))))

(define diff1-cont
  (lambda (exp2 env cont)
    (lambda (val1)
      (value-of/k exp2 env (diff2-cont val1 cont)))))

(define diff2-cont
  (lambda (val1 cont)
    (lambda (val2)
      (let ((num1 (expval->num val1))
            (num2 (expval->num val2)))
        (apply-cont cont
                    (num-val (- num1 num2)))))))

(define rator-cont
  (lambda (rand env cont)
    (lambda (val1)
      (value-of/k rand env
                  (rand-cont val1 cont)))))

(define rand-cont
  (lambda (val1 cont)
    (lambda (val2)
      (let ((proc1 (expval->proc val1)))
        (apply-procedure/k proc1 val2 cont)))))

(define apply-cont
  (lambda (cont v)
    (cont v)))

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
    (diff-exp (exp1 exp2)
              (value-of/k exp1 env
                          (diff1-cont exp2 env cont)))
    (zero?-exp (exp)
               (value-of/k exp env (zero?-cont cont)))
    (if-exp (cond exp-t exp-f)
            (value-of/k cond env (if-cont exp-t exp-f env cont)))
    (let-exp (var exp body)
             (value-of/k exp env (let-cont var body env cont)))
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

(define program-let "let x = 1 in x")
(equal?! program-let 1)

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
