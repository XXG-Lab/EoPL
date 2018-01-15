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

; BEGIN: Program
(define-datatype program program?
  (a-program (modules (list-of module-definition?))
             (exp expression?)))

(define (value-of-program pgm)
  (cases program pgm
    (a-program (defns body)
               (initialize-store!)
               (reject-same-module-name! defns)
               (value-of body
                         (add-module-defns-to-env defns (empty-env))))))

(define (type-of-program pgm)
  (cases program pgm
    (a-program (defns body)
               (type-of body
                        (add-module-defns-to-tenv defns (empty-tenv))))))

(define (run src)
  (let ((pgm (scan&parse src)))
    (cons (expval->val (value-of-program pgm))
          (type-to-external-form (type-of-program pgm)))))

; BEGIN: Value Type
(define identifier? symbol?)
(define reference? integer?)

(define-datatype expval expval?
  (num-val (num number?))
  (bool-val (bool boolean?))
  (proc-val (proc proc?))
  (ref-val (ref reference?)))

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

; BEGIN: Grammar
(define grammar
  '((program ((arbno module-definition) expression) a-program)
    (type ("int") int-type)
    (type ("bool") bool-type)
    (type ("(" type "->" type ")") proc-type)
    (expression (number) const-exp)
    (expression (identifier) var-exp)
    (expression ("-" "(" expression "," expression ")") diff-exp)
    (expression ("zero?" "(" expression ")") zero?-exp)
    (expression ("if" expression "then" expression "else" expression) if-exp)
    (expression ("proc" "(" identifier ":" type ")" expression) proc-exp)
    (expression ("let" identifier "=" expression "in" expression) let-exp)
    (expression ("letrec" type identifier "(" identifier ":" type ")" "=" expression
                 "in" expression) letrec-exp)
    (expression ("(" expression expression ")") call-exp)
    (expression ("from" identifier "take" identifier) qualified-var-exp)
    (module-definition ("module" identifier "interface" interface "body" module-body)
                       a-module-definition)
    (interface ("[" (arbno declaration) "]") an-interface)
    (declaration (identifier ":" type) a-declaration)
    (module-body ("[" (arbno definition) "]") a-module-body)
    (definition (identifier "=" expression) a-definition)))

; BEGIN: Module
(define-datatype module-definition module-definition?
  (a-module-definition (name identifier?)
                       (iface interface?)
                       (body module-body?)))

(define (module-definition->name d)
  (cases module-definition d
    (a-module-definition (name iface body) name)))

(define (reject-same-module-name! defns)
  (if (null? defns)
      '()
      (let ((current-name (module-definition->name (car defns))))
        (define (reject-rec defns)
          (if (null? defns)
              #t
              (let ((name (module-definition->name (car defns))))
                (if (eqv? name current-name)
                    (error 'reject-same-module-name!)
                    (reject-rec (cdr defns))))))
        (reject-rec (cdr defns))
        (reject-same-module-name! (cdr defns)))))

; BEGIN: Interface
(define-datatype interface interface?
  (an-interface (decls (list-of declaration?))))

; BEGIN: Declaration
(define-datatype declaration declaration?
  (a-declaration (var identifier?)
                 (ty type?)))

(define (declaration->name d)
  (cases declaration d
    (a-declaration (var ty) var)))

(define (declaration->type d)
  (cases declaration d
    (a-declaration (var ty) ty)))

; BEGIN: Module Body
(define-datatype module-body module-body?
  (a-module-body (defns (list-of definition?))))

; BEGIN: Definition
(define-datatype definition definition?
  (a-definition (var identifier?)
                (exp expression?)))

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
  (proc-exp (var identifier?)
            (ty type?)
            (body expression?))
  (let-exp (var identifier?)
           (exp expression?)
           (body expression?))
  (letrec-exp (output-type type?)
              (name identifier?)
              (var identifier?)
              (input-type type?)
              (exp expression?)
              (body expression?))
  (call-exp (rator expression?)
            (rans expression?))
  (qualified-var-exp (name identifier?)
                     (var identifier?)))

; BEGIN: Procedure
(define-datatype proc proc?
  (procedure (var identifier?)
             (body expression?)
             (saved-env environment?)))

(define (apply-procedure rator rand)
  (cases proc rator
    (procedure (var body saved-env)
               (value-of body (extend-env-val var rand)))))

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
  (extend-env-ref (var identifier?)
                  (ref reference?)
                  (saved-env environment?))
  (extend-env-with-module (name identifier?)
                          (val typed-module?)
                          (saved-env environment?)))

(define (apply-env-ref env search-var)
  (cases environment env
    (empty-env ()
               (error 'apply-env-ref "Unbound variable." search-var))
    (extend-env-ref (var ref saved-env)
                    (if (eqv? search-var var)
                        ref
                        (apply-env-ref saved-env search-var)))
    (extend-env-with-module (name val saved-env)
                            (apply-env-ref saved-env search-var))))

(define (apply-env-val env var)
  (deref (apply-env-ref env var)))

(define (lookup-module-name-in-env search-name env)
  (cases environment env
    (empty-env ()
               (error 'apply-env-ref "Unbound module." search-name))
    (extend-env-ref (var ref saved-env)
                    (lookup-module-name-in-env search-name saved-env))
    (extend-env-with-module (name val saved-env)
                            (if (eqv? name search-name)
                                val
                                (lookup-module-name-in-env search-name saved-env)))))

(define (lookup-qualified-var-in-env search-name search-var env)
  (let ((val (lookup-module-name-in-env search-name env)))
    (cases typed-module val
      (a-module (bindings)
                (apply-env-val bindings search-var)))))

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

; BEGIN: Type Environment
(define-datatype type-env type-env?
  (empty-tenv)
  (extend-tenv (var identifier?)
               (ty type?)
               (saved-tenv type-env?))
  (extend-tenv-with-module (name identifier?)
                           (interface interface?)
                           (saved-tenv type-env?)))

(define (extend-tenv* vars types tenv)
  (if (null? vars)
      tenv
      (extend-tenv* (cdr vars) (cdr types)
                    (extend-tenv (car vars) (car types) tenv))))

(define (apply-tenv tenv search-var)
  (cases type-env tenv
    (empty-tenv ()
                (eopl:error 'apply-tenv "Unbound identifier: " search-var))
    (extend-tenv (var ty saved-tenv)
                 (if (equal? var search-var)
                     ty
                     (apply-tenv saved-tenv search-var)))
    (extend-tenv-with-module (name interface saved-tenv)
                             (apply-tenv saved-tenv search-var))))

(define (lookup-module-name-in-tenv tenv search-name)
  (cases type-env tenv
    (empty-tenv ()
                (eopl:error 'apply-tenv "Unbound module: " search-name))
    (extend-tenv (var ty saved-tenv)
                 (lookup-module-name-in-tenv saved-tenv search-name))
    (extend-tenv-with-module (name interface saved-tenv)
                             (if (eqv? name search-name)
                                 interface
                                 (extend-tenv-with-module saved-tenv search-name)))))

(define (lookup-qualified-var-in-tenv search-name search-var tenv)
  (let ((iface (lookup-module-name-in-tenv tenv search-name)))
    (cases interface iface
      (an-interface (decls)
                    (lookup-variable-name-in-decls search-var decls)))))

(define (lookup-variable-name-in-decls search-var decls)
  (cases declaration (car decls)
    (a-declaration (var ty)
                   (if (eqv? search-var var)
                       ty
                       (lookup-variable-name-in-decls search-var (cdr decls))))))

(define (add-module-defns-to-tenv defns tenv)
  (if (null? defns)
      tenv
      (cases module-definition (car defns)
        (a-module-definition (name expected-iface body)
                             (let ((actual-iface (interface-of body tenv)))
                               (if (<:-iface actual-iface expected-iface tenv)
                                   (let ((next-tenv (extend-tenv-with-module name
                                                                             expected-iface
                                                                             tenv)))
                                     (add-module-defns-to-tenv (cdr defns) next-tenv))
                                   (error "Interface not satisfy: " actual-iface expected-iface)))))))

(define (interface-of body tenv)
  (cases module-body body
    (a-module-body (defns)
                   (an-interface (defns-to-decls defns tenv)))))

(define (defns-to-decls defns tenv)
  (if (null? defns)
      '()
      (cases definition (car defns)
        (a-definition (var exp)
                      (let ((ty (type-of exp tenv)))
                        (cons (a-declaration var ty)
                              (defns-to-decls (cdr defns)
                                (extend-tenv var ty tenv))))))))

(define (<:-iface iface1 iface2 tenv)
  (cases interface iface1
    (an-interface (decls1)
                  (cases interface iface2
                    (an-interface (decls2)
                                  (<:-decls decls1 decls2 tenv))))))

(define (<:-decls decls1 decls2 tenv)
  (cond ((null? decls2) #t)
        ((null? decls1) #f)
        (else (let ((name1 (declaration->name (car decls1)))
                    (name2 (declaration->name (car decls2))))
                (if (eqv? name1 name2)
                    (and (equal? (declaration->type (car decls1))
                                 (declaration->type (car decls2)))
                         (<:-decls (cdr decls1) (cdr decls2) tenv))
                    (<:-decls (cdr decls1) decls2 tenv))))))

; BEGIN: Typed Module
(define-datatype typed-module typed-module?
  (a-module (bindings environment?)))

(define (add-module-defns-to-env defns env)
  (if (null? defns)
      env
      (cases module-definition (car defns)
        (a-module-definition (name iface body)
                             (add-module-defns-to-env
                              (cdr defns)
                              (extend-env-with-module
                               name
                               (value-of-module-body body env)
                               env))))))

(define (value-of-module-body body env)
  (cases module-body body
    (a-module-body (defns)
                   (a-module (defns-to-env defns env)))))

(define (defns-to-env defns env)
  (if (null? defns)
      (empty-env)
      (cases definition (car defns)
        (a-definition (var exp)
                      (let* ((val (value-of exp env))
                             (next-env (extend-env-val var val env)))
                        (extend-env-val var val (defns-to-env (cdr defns) next-env)))))))

; BEGIN: Type
(define-datatype type type?
  (int-type)
  (bool-type)
  (proc-type (input type?)
             (output type?)))

(define (type-to-external-form ty)
  (cases type ty
    (int-type () 'int)
    (bool-type () 'bool)
    (proc-type (input output)
               (list (type-to-external-form input)
                     '->
                     (type-to-external-form output)))))

(define (check-equal-type! ty1 ty2 exp)
  (if (equal? ty1 ty2)
      #t
      (error 'check-equal-type! ty1 ty2 exp)))

(define (type-of exp tenv)
  (cases expression exp
    (const-exp (num)
               (int-type))
    (var-exp (var)
             (apply-tenv tenv var))
    (diff-exp (exp1 exp2)
              (let ((ty1 (type-of exp1 tenv))
                    (ty2 (type-of exp2 tenv)))
                (check-equal-type! ty1 (int-type) exp1)
                (check-equal-type! ty2 (int-type) exp2)
                (int-type)))
    (zero?-exp (exp)
               (let ((ty (type-of exp tenv)))
                 (check-equal-type! ty (int-type) exp)
                 (bool-type)))
    (if-exp (cond exp-t exp-f)
            (let ((ty (type-of cond tenv)))
              (check-equal-type! ty (bool-type) cond)
              (let ((ty1 (type-of exp-t tenv))
                    (ty2 (type-of exp-f tenv)))
                (check-equal-type! ty1 ty2 exp)
                ty2)))
    (proc-exp (var ty body)
              (proc-type ty (type-of body tenv)))
    (let-exp (var exp body)
             (let ((ty (type-of exp tenv)))
               (type-of body (extend-tenv var ty tenv))))
    (letrec-exp (output-type name var input-type exp body)
                (let ((body-tenv (extend-tenv name (proc-type input-type output-type) tenv)))
                  (let ((exp-type (type-of exp (extend-tenv var input-type body-tenv))))
                    (check-equal-type! exp-type output-type exp)
                    (type-of body body-tenv))))
    (call-exp (rator rand)
              (let ((rator-type (type-of rator tenv))
                    (rand-type (type-of rand tenv)))
                (cases type rator-type
                  (proc-type (arg-type result-type)
                             (begin (check-equal-type! arg-type rand-type rand)
                                    result-type))
                  (else (error "Rator is not a proc: " rator-type rator)))))
    (qualified-var-exp (name var)
                       (lookup-qualified-var-in-tenv name var tenv))))

; BEGIN: Evaluation
(define (value-of exp env)
  (cases expression exp
    (const-exp (num)
               (num-val num))
    (var-exp (var)
             (apply-env-val env var))
    (diff-exp (exp1 exp2)
              (let ((val1 (value-of exp1 env))
                    (val2 (value-of exp2 env)))
                (let ((num1 (expval->num val1))
                      (num2 (expval->num val2)))
                  (num-val (- num1 num2)))))
    (zero?-exp (exp)
               (let* ((val (value-of exp env))
                      (num (expval->num val)))
                 (bool-val (zero? num))))
    (if-exp (cond exp-t exp-f)
            (let ((val (value-of cond env)))
              (if (expval->bool val)
                  (value-of exp-t env)
                  (value-of exp-f env))))
    (proc-exp (var ty body)
              (proc-val (procedure var body env)))
    (let-exp (var exp body)
             (let ((val (value-of exp env)))
               (value-of body (extend-env-val var val env))))
    (letrec-exp (output-type name var input-type exp body)
                (value-of body (extend-env-rec name var exp env)))
    (call-exp (rator rand)
              (let ((proc (expval->proc (value-of rator env)))
                    (arg (value-of rand env)))
                (apply-procedure proc arg)))
    (qualified-var-exp (name var)
                       (lookup-qualified-var-in-env name var env))))

; BEGIN: Test
(define (equal?! prog expect)
  (let ((actual (run prog)))
    (display "Expect: ")
    (display expect)
    (display "\nActual: ")
    (display actual)
    (display "\n")
    (if (equal? actual expect)
        (display "\n")
        (begin (display prog)
               (display "\n")
               (display "Wrong Answer!!\n\n")))))

(define prog-1 "
module m1
  interface
    [a : int
     b : int
     c : int]
  body
    [a = 33
     x = -(a, 1)
     b = -(a, x)
     c = -(x, b)]
  let a = 10
  in -(-(from m1 take a,
         from m1 take b),
       a)")
(equal?! prog-1 (cons 22 'int))

(define prog-reject "
module m1
  interface
    [a : int]
  body
    [a = 33]
module m1
  interface
    [b : int]
  body
    [b = 33]
0")
(equal?! prog-reject '())
