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
  (a-program (exp expression?)))

(define (type-of-program pgm)
  (cases program pgm
    (a-program (exp)
               (type-of exp (empty-tenv)))))

(define (run pgm)
  (type-of-program (scan&parse pgm)))

; BEGIN: Value type
(define (identifier? x)
  (symbol? x))

; BEGIN: Type
(define-datatype type type?
  (int-type)
  (bool-type)
  (multi-type (types (list-of type?)))
  (proc-type (arg-type type?)
             (result-type type?)))

(define (check-equal-type! ty1 ty2 exp)
  (if (equal? ty1 ty2)
      #t
      (report-unequal-types ty1 ty2 exp)))

(define (report-unequal-types ty1 ty2 exp)
  (eopl:error 'checck-equal-type!
              "Types didn't match: ~s != ~a in~%~a"
              (type-to-external-form ty1)
              (type-to-external-form ty2)
              exp))

(define (multi-type-rec types)
  (if (null? types)
      '()
      (cons '* (cons (type-to-external-form (car types))
                     (multi-type-rec (cdr types))))))

(define (type-to-external-form ty)
  (cases type ty
    (int-type () 'int)
    (bool-type () 'bool)
    (multi-type (types)
                (if (equal? (length types) 1)
                    (type-to-external-form (car types))
                    (cdr (multi-type-rec types))))
    (proc-type (args-type result-type)
               (list (type-to-external-form args-type)
                     '->
                     (type-to-external-form result-type)))))

; BEGIN: Environment
(define-datatype type-env type-env?
  (empty-tenv)
  (extend-tenv (var identifier?)
               (ty type?)
               (saved-tenv type-env?)))

(define (extend-tenv* vars types tenv)
  (if (null? vars)
      tenv
      (extend-tenv* (cdr vars) (cdr types)
                    (extend-tenv (car vars) (car types) tenv))))

(define (extend-tenv-letrec names arg-typess result-types tenv)
  (if (null? names)
      tenv
      (extend-tenv-letrec (cdr names) (cdr arg-typess) (cdr result-types)
                          (extend-tenv (car names)
                                       (proc-type (multi-type (car arg-typess))
                                                  (car result-types))
                                       tenv))))

(define (apply-tenv tenv search-var)
  (cases type-env tenv
    (empty-tenv ()
                (eopl:error 'apply-tenv "Unbound identifier: " search-var))
    (extend-tenv (var ty saved-tenv)
                 (if (equal? var search-var)
                     ty
                     (apply-tenv saved-tenv search-var)))))

; BEGIN: Grammar
(define grammar
  '((program (expression) a-program)
    (type ("int") int-type)
    (type ("bool") bool-type)
    (type ("(" type "->" type ")") proc-type)
    (expression (number) const-exp)
    (expression (identifier) var-exp)
    (expression ("-" "(" expression "," expression ")") diff-exp)
    (expression ("zero?" "(" expression ")") zero?-exp)
    (expression ("if" expression "then" expression "else" expression) if-exp)
    (expression ("let" (arbno identifier "=" expression) "in" expression) let-exp)
    (expression ("proc" "(" (arbno identifier ":" type) ")" expression) proc-exp)
    (expression ("letrec" (arbno type identifier "(" (arbno identifier ":" type ) ")" "=" expression)
                 "in" expression) letrec-exp)
    (expression ("(" expression (arbno expression) ")") call-exp)))

; BEGIN: Expression
(define-datatype expression expression?
  (const-exp (num number?))
  (var-exp (var identifier?))
  (diff-exp (exp1 expression?)
            (exp2 expression?))
  (zero?-exp (exp1 expression?))
  (if-exp (cond expression?)
          (exp-t expression?)
          (exp-f expression?))
  (proc-exp (vars (list-of identifier?))
            (types (list-of type?))
            (body expression?))
  (let-exp (vars (list-of identifier?))
           (exps (list-of expression?))
           (body expression?))
  (letrec-exp (result-types (list-of type?))
              (names (list-of identifier?))
              (varss (list-of (list-of identifier?)))
              (var-types (list-of (list-of type?)))
              (exps (list-of expression?))
              (body expression?))
  (call-exp (rator expression?)
            (rands (list-of expression?))))

; BEGIN: Type-of
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
    (if-exp (exp1 exp2 exp3)
            (let ((ty1 (type-of exp1 tenv))
                  (ty2 (type-of exp2 tenv))
                  (ty3 (type-of exp3 tenv)))
              (check-equal-type! ty1 (bool-type) exp1)
              (check-equal-type! ty2 ty3 exp)
              ty2))
    (let-exp (vars exps body)
             (let ((types (map (lambda (exp) (type-of exp tenv)) exps)))
               (type-of body (extend-tenv* vars types tenv))))
    (letrec-exp (result-types names varss args-typess exps body)
                (type-of body (extend-tenv-letrec names args-typess result-types tenv)))
    (proc-exp (vars types body)
              (let ((result-type (type-of body (extend-tenv* vars types tenv))))
                (proc-type (multi-type types) result-type)))
    (call-exp (rator rands)
              (let ((rator-type (type-of rator tenv))
                    (rands-type (multi-type (map (lambda (exp) (type-of exp tenv)) rands))))
                (cases type rator-type
                  (proc-type (args-type result-type)
                             (begin (check-equal-type! args-type rands-type rands)
                                    result-type))
                  (else (error 'type-of "Not a proc." exp)))))
    (else (eopl:error 'type-of exp))))

; BEGIN: Test
(define (equal?! prog expect)
  (let ((actual (type-to-external-form (run prog))))
    (display "Expect: ")
    (display expect)
    (display "\nActual: ")
    (display actual)
    (display "\n")
    (if (equal? actual expect)
        (display "\n")
        (display "Wrong Answer!!\n\n"))))

(define program-const "1")
(equal?! program-const 'int)

(define program-zero "zero?(0)")
(equal?! program-zero 'bool)

(define program-let "let x = 0 in x")
(equal?! program-let 'int)

(define program-diff "let x = 0 y = 1 in -(x, y)")
(equal?! program-diff 'int)

(define program-if "if zero?(0) then 1 else 2")
(equal?! program-if 'int)

(define program-proc "proc(a : int b : int) -(a, b)")
(equal?! program-proc '((int * int) -> int))

(define program-call "(proc(a : int b : int) -(a, b) 2 4)")
(equal?! program-call 'int)

(define program-letrec "letrec int f(n : int) = -(n, 1) bool g(n : int) = zero?(n) in g")
(equal?! program-letrec '(int -> bool))
