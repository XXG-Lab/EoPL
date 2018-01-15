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
   (exp tf-exp?)))

(define (value-of-program pgm)
  (cases program pgm
    (a-program (exp)
               (value-of/k exp (empty-env) (end-cont)))))

(define (run prog)
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

(define (expval->num val)
  (cases expval val
    (num-val (num) num)
    (else (error 'num val))))

(define (expval->bool val)
  (cases expval val
    (bool-val (bool) bool)
    (else (error 'bool val))))

(define (expval->proc val)
  (cases expval val
    (proc-val (proc) proc)
    (else (error 'proc val))))

(define (expval->ref val)
  (cases expval val
    (ref-val (ref) ref)
    (else (error 'ref val))))

(define (expval->val val)
  (cases expval val
    (num-val (num) num)
    (bool-val (bool) bool)
    (proc-val (proc) proc)
    (ref-val (ref) (list 'ref ref))))

; BEGIN: Store
(define (store? x)
  (list-of x))

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
  (let ((next-env (extend-env-vals names
                                   (build-list (length names) (lambda (x) 'undefined))
                                   old-env)))
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
  '((program (tf-exp) a-program)
    (simple-exp (number) const-exp)
    (simple-exp (identifier) var-exp)
    (simple-exp ("-" "(" expression "," expression ")") cps-diff-exp)
    (simple-exp ("zero?" "(" expression ")") cps-zero?-exp)
    (simple-exp ("proc" "(" (arbno identifier) ")" expression) cps-proc-exp)
    (tr-exp (simple-exp) simple-exp->exp)
    (tf-exp ("let" (arbno identifier "=" expression) "in" expression) cps-let-exp)
    (tf-exp ("letrec" (arbno identifier "(" (arbno identifier) ")" "=" expression)
             "in" expression) cps-letrec-exp)
    (tf-exp ("if" expression "then" expression "else" expression) cps-if-exp)
    (tf-exp ("(" expression (arbno expression) ")") cps-call-exp)))

; BEGIN: Expression
(define-datatype simple-exp simple-exp?
  (const-exp (num number?))
  (var-exp (var identifier?))
  (cps-diff-exp (exp1 simple-exp?)
                (exp2 simple-exp?))
  (cps-zero?-exp (exp simple-exp?))
  (cps-proc-exp (vars (list-of identifier?))
                (body tf-exp?)))

(define-datatype tf-exp tf-exp?
  (simple-exp->exp (exp simple-exp?))
  (cps-let-exp (vars (list-of identifier?))
               (exps (list-of simple-exp?))
               (body tf-exp?))
  (cps-letrec-exp (names (list-of identifier?))
                  (varss (list-of (list-of identifier?)))
                  (exps (list-of tf-exp?))
                  (body tf-exp?))
  (cps-if-exp (cond simple-exp?)
              (exp-t tf-exp?)
              (exp-f tf-exp?))
  (cps-call-exp (rator simple-exp?)
                (rands (list-of simple-exp?))))

; BEGIN: Continuation
(define-datatype continuation continuation?
  (end-cont))

(define (apply-cont cont val)
  (cases continuation cont
    (end-cont () val)))

; BEGIN: Evaluation
(define (value-of/k exp env cont)
  (cases tf-exp exp
    (simple-exp->exp (simple)
                     (apply-cont cont (value-of-simple-exp simple env)))
    (cps-let-exp (var rhs body)
                 (let ((val (value-of-simple-exp rhs env)))
                   (value-of/k body (extend-env-vals (list var) (list val) env) cont)))
    (cps-letrec-exp (p-names b-varss p-bodies letrec-body)
                    (value-of/k letrec-body (extend-env-rec p-names b-varss p-bodies env) cont))
    (cps-if-exp (simple1 body1 body2)
                (if (expval->bool (value-of-simple-exp simple1 env))
                    (value-of/k body1 env cont)
                    (value-of/k body2 env cont)))
    (cps-call-exp (rator rands)
                  (let ((rator-proc (expval->proc (value-of-simple-exp rator env)))
                        (rand-vals (map (lambda (simple) (value-of-simple-exp simple env)) rands)))
                    (apply-procedure/k rator-proc rand-vals cont)))))

(define (value-of-simple-exp exp env)
  (cases simple-exp exp
    (const-exp (num)
                   (num-val num))
    (var-exp (var)
                 (apply-env-val env var))
    (cps-diff-exp (exp1 exp2)
                  (let ((val1 (expval->num (value-of-simple-exp exp1 env)))
                        (val2 (expval->num (value-of-simple-exp exp2 env))))
                    (num-val (- val1 val2))))
    (cps-zero?-exp (exp)
                   (bool-val (zero? (expval->num (value-of-simple-exp exp env)))))
    (cps-proc-exp (vars body)
                  (proc-val (procedure vars body env)))))

; BEGIN: Procedure
(define-datatype proc proc?
  (procedure (vars (list-of identifier?))
             (body tf-exp?)
             (saved-env environment?)))

(define apply-procedure/k
  (lambda (proc1 args cont)
    (cases proc proc1
      (procedure (vars body saved-env)
                 (value-of/k body (extend-env-vals vars args saved-env) cont)))))
