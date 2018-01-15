(require eopl)

; BEGIN: Scanner
(define scanner-spec
  '((white-sp (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier (letter (arbno (or letter digit "-" "!" "?"))) symbol)
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
    (type (identifier) named-type)
    (type ("from" identifier "take" identifier) qualified-type)
    (multi-type ("(" (separated-list type "*") ")") multi-type)
    (multi-type (arbno type) multi-type)
    (type ("(" multi-type "->" type ")") proc-type)
    (expression (number) const-exp)
    (expression (identifier) var-exp)
    (expression ("-" "(" expression "," expression ")") diff-exp)
    (expression ("zero?" "(" expression ")") zero?-exp)
    (expression ("if" expression "then" expression "else" expression) if-exp)
    (expression ("proc" "(" (arbno identifier ":" type) ")" expression) proc-exp)
    (expression ("let" (arbno identifier "=" expression) "in" expression) let-exp)
    (expression ("letrec" (arbno type identifier "(" (arbno identifier ":" type ) ")" "=" expression)
                 "in" expression) letrec-exp)
    (expression ("(" expression (arbno expression) ")") call-exp)
    (expression ("from" identifier "take" identifier) qualified-var-exp)
    (module-definition ("module" identifier "interface" interface "body" module-body)
                       a-module-definition)
    (interface ("[" (arbno declaration) "]") an-interface)
    (declaration (identifier ":" type) val-declaration)
    (declaration ("opaque" identifier) opaque-declaration)
    (declaration ("transparent" identifier "=" type) transparent-declaration)
    (module-body ("[" (arbno definition) "]") simple-module-body)
    (definition (identifier "=" expression) a-definition)
    (definition ("type" identifier "=" type) type-definition)))

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

(define (occure-in-interface? search-name iface)
  (cases interface iface
    (an-interface (decls)
                  (define (occur-rec decls)
                    (if (null? decls)
                        #f
                        (or (eqv? search-name (declaration->name (car decls)))
                            (occur-rec (cdr decls)))))
                  (occur-rec decls))))

; BEGIN: Declaration
(define-datatype declaration declaration?
  (val-declaration (var identifier?)
                   (ty type?))
  (opaque-declaration (name identifier?))
  (transparent-declaration (var identifier?)
                           (ty type?)))

(define (declaration->name d)
  (cases declaration d
    (val-declaration (var ty) var)
    (opaque-declaration (name) name)
    (transparent-declaration (var ty) var)))

(define (val-declaration? d)
  (cases declaration d
    (val-declaration (var ty) #t)
    (else #f)))

(define (opaque-declaration? d)
  (cases declaration d
    (opaque-declaration (name) #t)
    (else #f)))

(define (transparent-declaration? d)
  (cases declaration d
    (transparent-declaration (var ty) #t)
    (else #f)))

(define (declaration->type d)
  (cases declaration d
    (val-declaration (var ty) ty)
    (opaque-declaration (name) (error 'declaration->type d))
    (transparent-declaration (var ty) ty)))

; BEGIN: Module Body
(define-datatype module-body module-body?
  (simple-module-body (defns (list-of definition?))))

; BEGIN: Definition
(define-datatype definition definition?
  (a-definition (var identifier?)
                (exp expression?))
  (type-definition (var identifier?)
                   (type type?)))

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
  (proc-exp (vars (list-of identifier?))
            (types (list-of type?))
            (body expression?))
  (let-exp (vars (list-of identifier?))
           (exps (list-of expression?))
           (body expression?))
  (letrec-exp (output-types (list-of type?))
              (names (list-of identifier?))
              (varss (list-of (list-of identifier?)))
              (input-types (list-of (list-of type?)))
              (exps (list-of expression?))
              (body expression?))
  (call-exp (rator expression?)
            (rands (list-of expression?)))
  (qualified-var-exp (name identifier?)
                     (var identifier?)))

; BEGIN: Procedure
(define-datatype proc proc?
  (procedure (vars (list-of identifier?))
             (body expression?)
             (saved-env environment?)))

(define (apply-procedure rator vals)
  (cases proc rator
    (procedure (vars body saved-env)
               (value-of body (extend-env-vals vars vals saved-env)))))

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

(define (exist? var lst)
  (if (null? lst)
      #f
      (or (eqv? var (car lst))
          (exist? var (cdr lst)))))

; BEGIN: Type Environment
(define-datatype type-env type-env?
  (empty-tenv)
  (extend-tenv (var identifier?)
               (ty type?)
               (saved-tenv type-env?))
  (extend-tenv-with-module (name identifier?)
                           (interface interface?)
                           (saved-tenv type-env?))
  (extend-tenv-with-type (name identifier?)
                         (type type?)
                         (saved-tenv type-env?)))

(define (extend-tenv* vars types tenv)
  (if (null? vars)
      tenv
      (extend-tenv* (cdr vars) (cdr types)
                    (extend-tenv (car vars) (expand-type (car types) tenv) tenv))))

(define (apply-tenv tenv search-var)
  (cases type-env tenv
    (empty-tenv ()
                (eopl:error 'apply-tenv "Unbound identifier: " search-var))
    (extend-tenv (var ty saved-tenv)
                 (if (equal? var search-var)
                     ty
                     (apply-tenv saved-tenv search-var)))
    (extend-tenv-with-module (name interface saved-tenv)
                             (apply-tenv saved-tenv search-var))
    (extend-tenv-with-type (name type saved-tenv)
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
                                 (extend-tenv-with-module saved-tenv search-name)))
    (extend-tenv-with-type (name type saved-tenv)
                           (lookup-module-name-in-tenv saved-tenv search-name))))

(define (lookup-qualified-var-in-tenv search-name search-var tenv)
  (let ((iface (lookup-module-name-in-tenv tenv search-name)))
    (cases interface iface
      (an-interface (decls)
                    (lookup-variable-name-in-decls search-var decls)))))

(define (lookup-variable-name-in-decls search-var decls)
  (cases declaration (car decls)
    (val-declaration (var ty)
                   (if (eqv? search-var var)
                       ty
                       (lookup-variable-name-in-decls search-var (cdr decls))))
    (opaque-declaration (name)
                        (lookup-variable-name-in-decls search-var (cdr decls)))
    (transparent-declaration (name type)
                             (lookup-variable-name-in-decls search-var (cdr decls)))))

(define (expand-type ty tenv)
  (cases type ty
    (int-type () (int-type))
    (bool-type () (bool-type))
    (named-type (name)
                (lookup-type-name-in-tenv name tenv))
    (qualified-type (name var)
                    (lookup-qualified-type-in-tenv name var tenv))
    (multi-type (types)
                (multi-type (map (lambda (ty) (expand-type ty tenv)) types)))
    (proc-type (input output)
               (proc-type (expand-type input tenv)
                          (expand-type output tenv)))))

(define (lookup-type-name-in-tenv search-name tenv)
  (cases type-env tenv
    (empty-tenv ()
                (error 'lookup-type-name-in-tenv "Unbound name: " search-name))
    (extend-tenv (var ty saved-tenv)
                 (lookup-type-name-in-tenv search-name saved-tenv))
    (extend-tenv-with-module (name interface saved-tenv)
                             (lookup-type-name-in-tenv search-name saved-tenv))
    (extend-tenv-with-type (name type saved-tenv)
                           (if (eqv? name search-name)
                               type
                               (lookup-type-name-in-tenv search-name saved-tenv)))))

(define (lookup-qualified-type-in-decls search-var decls)
  (if (null? decls)
      (eopl:error 'lookup-qualified-type-in-decls "Unbound type: " search-var)
      (cases declaration (car decls)
        (val-declaration (name ty)
                         (lookup-qualified-type-in-decls search-var (cdr decls)))
        (opaque-declaration (name)
                            (lookup-qualified-type-in-decls search-var (cdr decls)))
        (transparent-declaration (name type)
                                 (if (eqv? name search-var)
                                     type
                                     (lookup-qualified-type-in-decls search-var (cdr decls)))))))

(define (lookup-qualified-type-in-tenv search-name search-var tenv)
  (cases type-env tenv
    (empty-tenv ()
                (eopl:error 'lookup-qualified-type-in-tenv "Unbound module: " search-name))
    (extend-tenv (var ty saved-tenv)
                 (lookup-qualified-type-in-tenv search-name search-var saved-tenv))
    (extend-tenv-with-module (name iface saved-tenv)
                             (if (eqv? name search-name)
                                 (cases interface iface
                                   (an-interface (decls)
                                                 (lookup-qualified-type-in-decls search-var decls)))
                                 (lookup-qualified-type-in-tenv search-name search-var saved-tenv)))
    (extend-tenv-with-type (name type saved-tenv)
                           (lookup-qualified-type-in-tenv search-name search-var saved-tenv))))

(define (add-module-defns-to-tenv defns tenv)
  (if (null? defns)
      tenv
      (cases module-definition (car defns)
        (a-module-definition (name expected-iface body)
                             (let ((actual-iface (interface-of body tenv)))
                               (if (<:-iface actual-iface expected-iface tenv)
                                   (let ((next-tenv (extend-tenv-with-module
                                                     name
                                                     (expand-iface name expected-iface tenv)
                                                     tenv)))
                                     (add-module-defns-to-tenv (cdr defns) next-tenv))
                                   (error "Interface not satisfy: " actual-iface expected-iface)))))))

(define (interface-of body tenv)
  (cases module-body body
    (simple-module-body (defns)
                        (an-interface (defns-to-decls defns tenv)))))

(define (expand-iface name iface tenv)
  (cases interface iface
    (an-interface (decls)
                  (an-interface (expand-decls name decls tenv)))))

(define (expand-decls name decls internal-tenv)
  (if (null? decls)
      '()
      (cases declaration (car decls)
        (val-declaration (var ty)
                         (let ((expanded-type (expand-type ty internal-tenv)))
                           (cons
                            (val-declaration var expanded-type)
                            (expand-decls name (cdr decls) internal-tenv))))
        (opaque-declaration (var)
                            (let ((expanded-type (qualified-type name var)))
                              (let ((new-env (extend-tenv-with-type var expanded-type internal-tenv)))
                                (cons
                                 (transparent-declaration var expanded-type)
                                 (expand-decls name (cdr decls) new-env)))))
        (transparent-declaration (var ty)
                                 (let ((expanded-type (expand-type ty internal-tenv)))
                                   (let ((new-env (extend-tenv-with-type var
                                                                         expanded-type
                                                                         internal-tenv)))
                                     (cons
                                      (transparent-declaration var expanded-type)
                                      (expand-decls name (cdr decls) new-env))))))))

(define (defns-to-decls defns tenv)
  (if (null? defns)
      '()
      (cases definition (car defns)
        (a-definition (var exp)
                      (let ((ty (type-of exp tenv)))
                        (cons (val-declaration var ty)
                              (defns-to-decls (cdr defns)
                                (extend-tenv var ty tenv)))))
        (type-definition (name type)
                         (cons (transparent-declaration name type)
                               (defns-to-decls (cdr defns) (extend-tenv-with-type
                                                            name
                                                            (expand-type type tenv)
                                                            tenv)))))))

(define (<:-iface iface1 iface2 tenv)
  (cases interface iface1
    (an-interface (decls1)
                  (cases interface iface2
                    (an-interface (decls2)
                                  (<:-decls decls1 decls2 tenv))))))

(define (equiv-type? ty1 ty2 tenv)
  (equal? (expand-type ty1 tenv)
          (expand-type ty2 tenv)))

(define (<:-decl decl1 decl2 tenv)
  (or (and (val-declaration? decl1)
           (val-declaration? decl2)
           (equiv-type? (declaration->type decl1)
                        (declaration->type decl2) tenv))
      (and (transparent-declaration? decl1)
           (transparent-declaration? decl2)
           (equiv-type? (declaration->type decl1)
                        (declaration->type decl2) tenv))
      (and (transparent-declaration? decl1)
           (opaque-declaration? decl2))
      (and (opaque-declaration? decl1)
           (opaque-declaration? decl2))))

(define (<:-decls decls1 decls2 tenv)
  (cond ((null? decls2) #t)
        ((null? decls1) #f)
        (else
         (let ((name1 (declaration->name (car decls1)))
               (name2 (declaration->name (car decls2))))
           (if (eqv? name1 name2)
               (and (<:-decl (car decls1) (car decls2) tenv)
                    (<:-decls (cdr decls1) (cdr decls2)
                              (extend-tenv-with-decl (car decls1) tenv)))
               (<:-decls (cdr decls1) decls2
                         (extend-tenv-with-decl (car decls1) tenv)))))))

(define fresh-module-name
  (let ((sn 0))
    (lambda (identifier)
      (set! sn (+ sn 1))
      (string->symbol
       (string-append (symbol->string identifier) "%" (number->string sn))))))

(define (extend-tenv-with-decl decl tenv)
  (cases declaration decl
    (val-declaration (name type)
                     tenv)
    (transparent-declaration (name type)
                             (extend-tenv-with-type name (expand-type type tenv) tenv))
    (opaque-declaration (name)
                        (extend-tenv-with-type name (qualified-type (fresh-module-name '%unknow) name)
                                               tenv))))

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
                               (value-of-module-body iface body env)
                               env))))))

(define (value-of-module-body iface body env)
  (cases module-body body
    (simple-module-body (defns)
                        (a-module (defns-to-env iface defns env)))))

(define (defns-to-env iface defns env)
  (if (null? defns)
      (empty-env)
      (cases definition (car defns)
        (a-definition (var exp)
                      (let* ((val (value-of exp env))
                             (next-env (extend-env-val var val env)))
                        (if (occure-in-interface? var iface)
                            (extend-env-val var val (defns-to-env iface (cdr defns) next-env))
                            (defns-to-env iface (cdr defns) next-env))))
        (type-definition (name type)
                         (defns-to-env iface (cdr defns) env)))))

; BEGIN: Type
(define-datatype type type?
  (int-type)
  (bool-type)
  (named-type (name identifier?))
  (qualified-type (name identifier?)
                  (var identifier?))
  (multi-type (types (list-of type?)))
  (proc-type (input type?)
             (output type?)))

(define (multi-type-rec types)
  (if (null? types)
      '()
      (cons '* (cons (type-to-external-form (car types))
                     (multi-type-rec (cdr types))))))

(define (type-to-external-form ty)
  (cases type ty
    (int-type () 'int)
    (bool-type () 'bool)
    (named-type (name) name)
    (qualified-type (name var)
                    (list 'from name 'take var)) 
    (multi-type (types)
                (if (equal? (length types) 1)
                    (type-to-external-form (car types))
                    (cdr (multi-type-rec types))))
    (proc-type (input output)
               (list (type-to-external-form input)
                     '->
                     (type-to-external-form output)))))

(define (check-equal-type! ty1 ty2 tenv exp)
  (let ((type1 (expand-type ty1 tenv))
        (type2 (expand-type ty2 tenv)))
    (if (equal? type1 type2)
        #t
        (error 'check-equal-type! type1 type2 exp))))

(define (map-of-two op a b)
  (if (null? a)
      '()
      (cons (op (car a) (car b))
            (map-of-two op (cdr a) (cdr b)))))

(define (type-of exp tenv)
  (cases expression exp
    (const-exp (num)
               (int-type))
    (var-exp (var)
             (apply-tenv tenv var))
    (diff-exp (exp1 exp2)
              (let ((ty1 (type-of exp1 tenv))
                    (ty2 (type-of exp2 tenv)))
                (check-equal-type! ty1 (int-type) tenv exp1)
                (check-equal-type! ty2 (int-type) tenv exp2)
                (int-type)))
    (zero?-exp (exp)
               (let ((ty (type-of exp tenv)))
                 (check-equal-type! ty (int-type) tenv exp)
                 (bool-type)))
    (if-exp (cond exp-t exp-f)
            (let ((ty (type-of cond tenv)))
              (check-equal-type! ty (bool-type) tenv cond)
              (let ((ty1 (type-of exp-t tenv))
                    (ty2 (type-of exp-f tenv)))
                (check-equal-type! ty1 ty2 tenv exp)
                ty2)))
    (proc-exp (vars types body)
              (let ((result-type (type-of body (extend-tenv* vars types tenv))))
                (proc-type (expand-type (multi-type types) tenv) result-type)))
    (let-exp (vars exps body)
             (let ((types (map (lambda (exp) (type-of exp tenv)) exps)))
               (type-of body (extend-tenv* vars types tenv))))
    (letrec-exp (result-types names varss arg-typess exps body)
                (let* ((types (map-of-two (lambda (arg-types result-type)
                                            (proc-type (expand-type (multi-type arg-types) tenv)
                                                       (expand-type result-type tenv)))
                                          arg-typess
                                          result-types))
                       (body-tenv (extend-tenv* names types tenv)))
                  (define (check-rec varss arg-typess result-types exps)
                    (if (null? exps)
                        #t
                        (let ((exp-type (type-of (car exps) (extend-tenv* (car varss)
                                                                          (car arg-typess)
                                                                         body-tenv))))
                          (check-equal-type! (car result-types) exp-type tenv (car exps))
                          (check-rec (cdr varss) (cdr arg-typess) (cdr result-types) (cdr exps)))))
                  (check-rec varss arg-typess result-types exps)
                  (type-of body body-tenv)))
    (call-exp (rator rands)
              (let ((rator-type (type-of rator tenv))
                    (rands-type (multi-type (map (lambda (exp) (type-of exp tenv)) rands))))
                (cases type rator-type
                  (proc-type (args-type result-type)
                             (begin (check-equal-type! args-type rands-type tenv rands)
                                    result-type))
                  (else (error 'type-of "Not a proc." exp)))))
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
    (proc-exp (vars types body)
              (proc-val (procedure vars body env)))
    (let-exp (vars exps body)
             (let ((vals (map (lambda (exp) (value-of exp env)) exps)))
               (value-of body (extend-env-vals vars vals env))))
    (letrec-exp (output-types names varss input-typess exps body)
                (value-of body (extend-env-rec names varss exps env)))
    (call-exp (rator rands)
              (let ((proc (expval->proc (value-of rator env)))
                    (args (map (lambda (exp) (value-of exp env)) rands)))
                (apply-procedure proc args)))
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
        (begin ;(display prog)
               ;(display "\n")
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
     a)
")
(equal?! prog-1 (cons 22 'int))

(define prog-2 "
module m1
  interface
    [transparent t = int
     z : t
     s : ((t) -> t)
     is-z? : ((t) -> bool)]
  body
    [type t = int
     z = 33
     s = proc (x : t) -(x,-1)
     is-z? = proc (x : t) zero?(-(x,z))]
proc (x : from m1 take t)
  (from m1 take is-z? -(x,0))
")
(equal? (cdr (run prog-2)) '(int -> bool))

(define prog-3 "
module m1
  interface
    [opaque t
     z : t
     s : ((t) -> t)
     is-z? : ((t) -> bool)]
   body
    [type t = int
     z = 33
     s = proc (x : t) -(x,-1)
     is-z? = proc (x : t) zero?(-(x,z))]
proc (x : from m1 take t)
  (from m1 take is-z? x)
")
(equal? (cdr (run prog-3)) '((from m1 take t) -> bool))

(define prog-4 "
module ints1
  interface
    [opaque t
     zero : t
     succ : ((t) -> t)
     pred : ((t) -> t)
     is-zero : ((t) -> bool)]
  body
    [type t = int
     zero = 0
     succ = proc(x : t) -(x,-5)
     pred = proc(x : t) -(x,5)
     is-zero = proc (x : t) zero?(x)]
let z = from ints1 take zero
in let s = from ints1 take succ
    in (s (s z))
")
(equal?! prog-4 (cons 10 '(from ints1 take t)))

(define prog-5 "
module ints2
  interface
    [opaque t
     zero : t
     succ : ((t) -> t)
     pred : ((t) -> t)
     is-zero : ((t) -> bool)]
  body
    [type t = int
     zero = 0
     succ = proc(x : t) -(x,3)
     pred = proc(x : t) -(x,-3)
     is-zero = proc (x : t) zero?(x)]
let z = from ints2 take zero
in let s = from ints2 take succ
   in (s (s z))
")
(equal?! prog-5 (cons -6 '(from ints2 take t)))

(define prog-6 "
module ints1
  interface
    [opaque t
     zero : t
     succ : ((t) -> t)
     pred : ((t) -> t)
     is-zero : ((t) -> bool)]
  body
    [type t = int
     zero = 0
     succ = proc(x : t) -(x,-5)
     pred = proc(x : t) -(x,5)
     is-zero = proc (x : t) zero?(x)]
let z = from ints1 take zero
in let s = from ints1 take succ
in let p = from ints1 take pred
in let z? = from ints1 take is-zero
in letrec int to-int (x : from ints1 take t) =
                if (z? x)
                then 0
                else -((to-int (p x)), -1)
in (to-int (s (s z)))
")
(equal?! prog-6 (cons 2 'int))

(define prog-7 "
module ints2
  interface
    [opaque t
     zero : t
     succ : ((t) -> t)
     pred : ((t) -> t)
     is-zero : ((t) -> bool)]
  body
    [type t = int
     zero = 0
     succ = proc(x : t) -(x,3)
     pred = proc(x : t) -(x,-3)
     is-zero = proc (x : t) zero?(x)]
let z = from ints2 take zero
in let s = from ints2 take succ
in let p = from ints2 take pred
in let z? = from ints2 take is-zero
in letrec int to-int (x : from ints2 take t) =
                if (z? x)
                then 0
                else -((to-int (p x)), -1)
in (to-int (s (s z)))
")
(equal?! prog-7 (cons 2 'int))

(define prog-8 "
module mybool
  interface
    [opaque t
     true : t
     false : t
     and : ((t * t) -> t)
     not : ((t) -> t)
     to-bool : ((t) -> bool)]
  body
    [type t = int
     true = 0
     false = 13
     and = proc (x : t y : t)
             if zero?(x) then y else false
     not = proc (x : t)
             if zero?(x) then false else true
     to-bool = proc (x : t) zero?(x)]
let true = from mybool take true
in let false = from mybool take false
in let and = from mybool take and
in (and true false)
")
(equal?! prog-8 (cons 13 '(from mybool take t)))
