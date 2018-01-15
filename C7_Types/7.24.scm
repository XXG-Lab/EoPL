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
    (a-program (exp1)
               (initialize-store!)
               (initialize-subst!)
               (apply-subst-to-type (type-of exp1 (empty-tenv))))))

(define (run pgm)
  (type-to-external-form
   (type-of-program
    (scan&parse pgm))))

; BEGIN: Value type
(define identifier? symbol?)
(define reference? integer?)

; BEGIN: Type
(define-datatype type type?
  (tvar-type (serial-number integer?))
  (void-type)
  (int-type)
  (bool-type)
  (pair-type (ty1 type?)
             (ty2 type?))
  (multi-type (types (list-of type?)))
  (proc-type (arg-type type?)
             (result-type type?)))

(define (tvar-type? ty)
  (cases type ty
    (tvar-type (n) #t)
    (else #f)))

(define (proc-type? ty)
  (cases type ty
    (proc-type (t1 t2) #t)
    (else #f)))

(define (proc-type->arg-type ty)
  (cases type ty
    (proc-type (arg-type result-type) arg-type)
    (else (error 'proc-type->arg-type ty))))

(define (proc-type->result-type ty)
  (cases type ty
    (proc-type (arg-type result-type) result-type)
    (else (error 'proc-type->result-type ty))))

(define (pair-type? ty)
  (cases type ty
    (pair-type (t1 t2) #t)
    (else #f)))

(define (pair-type->left-type ty)
  (cases type ty
    (pair-type (ty1 ty2) ty1)
    (else (error 'pair-type->left-type ty))))

(define (pair-type->right-type ty)
  (cases type ty
    (pair-type (ty1 ty2) ty2)
    (else (error 'pair-type->right-type ty))))

(define (multi-type? ty)
  (cases type ty
    (multi-type (types) #t)
    (else #f)))

(define (multi-type->types ty)
  (cases type ty
    (multi-type (types) types)
    (else (error 'multi-type->types ty))))

(define fresh-tvar-type
  (let ((sn 0))
    (lambda ()
      (set! sn (+ sn 1))
      (tvar-type sn))))

(define (otype->type ty)
  (cases otype ty
    (no-type () (fresh-tvar-type))
    (a-type (ty) ty)))

(define-datatype otype otype?
  (no-type)
  (a-type (ty type?)))

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
    (tvar-type (serial-number)
               (list 'var serial-number))
    (void-type () 'void)
    (int-type () 'int)
    (bool-type () 'bool)
    (pair-type (ty1 ty2)
               (list (type-to-external-form ty1)
                     '*
                     (type-to-external-form ty2)))
    (multi-type (types)
                (if (equal? (length types) 1)
                    (type-to-external-form (car types))
                    (cdr (multi-type-rec types))))
    (proc-type (args-type result-type)
               (list (type-to-external-form args-type)
                     '->
                     (type-to-external-form result-type)))))

; BEGIN: Substitution
(define global-subst 'uninitialized)

(define (initialize-subst!)
  (set! global-subst (empty-subst)))

(define-datatype substitution substitution?
  (empty-subst)
  (extended-subst (subst substitution?)
                  (var tvar-type?)
                  (ty reference?)))

(define (apply-subst search-var)
  (define (apply-subst-rec subst)
    (cases substitution subst
      (empty-subst ()
                   #f)
      (extended-subst (subst var ty)
                      (if (equal? search-var var)
                          ty
                          (apply-subst-rec subst)))))
  (apply-subst-rec global-subst))

(define (extend-subst var val)
  (set! global-subst (extended-subst global-subst var (newref val))))

(define (apply-one-subst ty0 tvar ty1)
  (cases type ty0
    (void-type () (void-type))
    (int-type () (int-type))
    (bool-type () (bool-type))
    (pair-type (type1 type2)
               (pair-type (apply-one-subst type1 tvar ty1)
                          (apply-one-subst type2 tvar ty1)))
    (multi-type (types)
                (multi-type (map (lambda (ty0) (apply-one-subst ty0 tvar ty1)) types)))
    (proc-type (arg-type result-type)
               (proc-type (apply-one-subst arg-type tvar ty1)
                          (apply-one-subst result-type tvar ty1)))
    (tvar-type (sn)
               (if (equal? ty0 tvar) ty1 ty0))))

(define (apply-subst-to-type ty)
  (cases type ty
    (void-type () (void-type))
    (int-type () (int-type))
    (bool-type () (bool-type))
    (pair-type (t1 t2)
               (pair-type (apply-subst-to-type t1)
                          (apply-subst-to-type t2)))
    (multi-type (types)
                (multi-type (map apply-subst-to-type types)))
    (proc-type (t1 t2)
               (proc-type (apply-subst-to-type t1)
                          (apply-subst-to-type t2)))
    (tvar-type (sn)
               (let ((tmp (apply-subst ty)))
                 (if tmp
                     (let ((next-type (apply-subst-to-type (deref tmp))))
                       (setref! tmp next-type)
                       next-type)
                     ty)))))

(define (map-of-two op a b)
  (if (null? a)
      '()
      (cons (op (car a) (car b))
            (map-of-two op (cdr a) (cdr b)))))

(define (unifier ty1 ty2 exp)
  (let ((ty1 (if (tvar-type? ty1) (apply-subst-to-type ty1) ty1))
        (ty2 (if (tvar-type? ty2) (apply-subst-to-type ty2) ty2)))
    (cond ((equal? ty1 ty2))
          ((tvar-type? ty1)
           (if (no-occurrence? ty1 ty2)
               (extend-subst ty1 ty2)
               (error 'no-occurrence-violation ty1 ty2 exp)))
          ((tvar-type? ty2)
           (if (no-occurrence? ty2 ty1)
               (extend-subst ty2 ty1)
               (error 'no-occurrence-violation ty1 ty2 exp)))
          ((and (proc-type? ty1) (proc-type? ty2))
           (unifier (proc-type->arg-type ty1) (proc-type->arg-type ty2) exp)
           (unifier (proc-type->result-type ty1) (proc-type->result-type ty2) exp))
          ((and (pair-type? ty1) (pair-type? ty2))
           (unifier (pair-type->left-type ty1) (pair-type->left-type ty2) exp)
           (unifier (pair-type->right-type ty1) (pair-type->right-type ty2) exp))
          ((and (multi-type? ty1) (multi-type? ty2))
           (map-of-two (lambda (ty1 ty2) (unifier ty1 ty2 exp))
                       (multi-type->types ty1)
                       (multi-type->types ty2)))
          (else (error 'unification-failure ty1 ty2 exp)))))

(define (no-occurrence? tvar ty)
  (cases type ty
    (void-type () #t)
    (int-type () #t)
    (bool-type () #t)
    (pair-type (ty1 ty2)
               (and (no-occurrence? tvar ty1)
                    (no-occurrence? tvar ty2)))
    (multi-type (types)
                (and (map (lambda (ty) (no-occurrence? tvar ty)) types)))
    (proc-type (arg-type result-type)
               (and (no-occurrence? tvar arg-type)
                    (no-occurrence? tvar result-type)))
    (tvar-type (serial-number) (not (equal? tvar ty)))))

; BEGIN: Store
(define store? list?)

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
    (otype ("?") no-type)
    (otype (type) a-type)
    (expression (number) const-exp)
    (expression (identifier) var-exp)
    (expression ("newpair" "(" expression "," expression ")") pair-exp)
    (expression ("unpair" identifier identifier "=" expression "in" expression) unpair-exp)
    (expression ("-" "(" expression "," expression ")") diff-exp)
    (expression ("zero?" "(" expression ")") zero?-exp)
    (expression ("if" expression "then" expression "else" expression) if-exp)
    (expression ("let" (arbno identifier "=" expression) "in" expression) let-exp)
    (expression ("proc" "(" (arbno identifier ":" otype) ")" expression) proc-exp)
    (expression ("letrec" (arbno otype identifier "(" (arbno identifier ":" otype) ")" "=" expression)
                 "in" expression) letrec-exp)
    (expression ("(" expression (arbno expression) ")") call-exp)))

; BEGIN: Expression
(define-datatype expression expression?
  (const-exp (num number?))
  (var-exp (var identifier?))
  (pair-exp (exp1 expression?)
            (exp2 expression?))
  (unpair-exp (var1 identifier?)
              (var2 identifier?)
              (exp expression?)
              (body expression?))
  (diff-exp (exp1 expression?)
            (exp2 expression?))
  (zero?-exp (exp1 expression?))
  (if-exp (cond expression?)
          (exp-t expression?)
          (exp-f expression?))
  (proc-exp (vars (list-of identifier?))
            (types (list-of otype?))
            (body expression?))
  (let-exp (vars (list-of identifier?))
           (exps (list-of expression?))
           (body expression?))
  (letrec-exp (result-types (list-of otype?))
              (names (list-of identifier?))
              (varss (list-of (list-of identifier?)))
              (var-typess (list-of (list-of otype?)))
              (exps (list-of expression?))
              (body expression?))
  (call-exp (rator expression?)
            (rands (list-of expression?))))

; BEGIN: Type-of
(define (map-of-three op a b c)
  (if (null? a)
      '()
      (cons (op (car a) (car b) (car c))
            (map-of-three op (cdr a) (cdr b) (cdr c)))))

(define (type-of exp tenv)
  (cases expression exp
    (const-exp (num)
               (int-type))
    (var-exp (var)
             (apply-tenv tenv var))
    (pair-exp (exp1 exp2)
              (let ((ty1 (type-of exp1 tenv))
                    (ty2 (type-of exp2 tenv)))
                (pair-type ty1 ty2)))
    (unpair-exp (var1 var2 exp body)
                (let ((ty (type-of exp tenv)))
                  (cases type ty
                    (pair-type (ty1 ty2)
                               (type-of body (extend-tenv var1 ty1 (extend-tenv var2 ty2 tenv))))
                    (else (error 'type-of "Expression should be a pair: " exp)))))
    (zero?-exp (exp)
               (unifier (type-of exp tenv) (int-type) exp)
               (bool-type))
    (diff-exp (exp1 exp2)
              (let ((ty1 (type-of exp1 tenv)))
                (unifier ty1 (int-type) exp1)
                (let ((ty2 (type-of exp2 tenv)))
                  (unifier ty2 (int-type) exp2)
                  (int-type))))
    (if-exp (exp1 exp2 exp3)
            (let ((ty1 (type-of exp1 tenv)))
              (unifier ty1 (bool-type) exp1)
              (let* ((ty2 (type-of exp2 tenv))
                     (ty3 (type-of exp3 tenv)))
                (unifier ty2 ty3 exp)
                ty2)))
    (let-exp (vars exps body)
             (let ((types (map (lambda (exp) (type-of exp tenv)) exps)))
               (type-of body (extend-tenv* vars types tenv))))
    (proc-exp (vars otypes body)
              (let* ((types (map otype->type otypes))
                     (body-type (type-of body (extend-tenv* vars types tenv))))
                (proc-type (multi-type types) body-type)))
    (letrec-exp (result-otypes names varss var-otypess exps body)
                (let* ((result-types (map otype->type result-otypes))
                       (var-typess (map (lambda (types) (map otype->type types)) var-otypess))
                       (body-tenv (extend-tenv* names
                                                (map-of-two (lambda (var-types result-type)
                                                              (proc-type (multi-type var-types)
                                                                         result-type))
                                                            var-typess
                                                            result-types)
                                                tenv))
                       (exp-types (map-of-three (lambda (vars types exp)
                                                  (type-of exp (extend-tenv* vars types body-tenv)))
                                                varss
                                                var-typess
                                                exps)))
                  (map-of-two (lambda (ty1 ty2) (unifier ty1 ty2 exp)) exp-types result-types)
                  (type-of body body-tenv)))
    (call-exp (rator rands)
              (let* ((result-type (fresh-tvar-type))
                     (rator-type (type-of rator tenv))
                     (rands-type (map (lambda (exp) (type-of exp tenv)) rands)))
                (unifier rator-type (proc-type (multi-type rands-type) result-type) exp)
                result-type))))

; BEGIN: Tests
(define (equal?! prog expect)
  (let ((actual (run prog)))
    (display "Expect: ")
    (display expect)
    (display "\nActual: ")
    (display actual)
    (display "\n")
    (if (equal? actual expect)
        (display "\n")
        (display "Wrong Answer!!\n\n"))))

(define prog-1-1 "proc (x : ?) -(x,3)")
(equal?! prog-1-1 '(int -> int))

(define prog-1-2 "proc (f:?) proc (x:?) -((f x), 1)")
(equal?! prog-1-2 '(((var 3) -> int) -> ((var 3) -> int)))

(define prog-1-3 "proc (x:?) x")
(equal?! prog-1-3 '((var 5) -> (var 5)))

(define prog-1-4 "proc (x:?) proc (y:?) (x y)")
(equal?! prog-1-4 '(((var 7) -> (var 8)) -> ((var 7) -> (var 8))))

(define prog-1-5 "proc (x:?) (x 3)")
(equal?! prog-1-5 '((int -> (var 10)) -> (var 10)))

(define prog-1-7 "proc (x:?) if x then 88 else 99")
(equal?! prog-1-7 '(bool -> int))

(define prog-1-8 "proc (x:?) proc (y:?) if x then y else 99")
(equal?! prog-1-8 '(bool -> (int -> int)))

(define prog-1-11 "proc(f:?)proc(g:?)proc(p:?)proc(x:?) if (p (f x)) then (g 1) else -((f x),1)")
(equal?! prog-1-11 '(((var 17) -> int) -> ((int -> int) -> ((int -> bool) -> ((var 17) -> int)))))

(define prog-1-12 "proc(x:?)proc(p:?)proc(f:?)if (p x) then -(x,1) else (f p)")
(equal?! prog-1-12 '(int -> ((int -> bool) -> (((int -> bool) -> int) -> int))))

(define prog-13-2 "let f = proc (z:?) z in proc (x:?) -((f x), 1)")
(equal?! prog-13-2 '(int -> int))

(define prog-13-3 "let p = zero?(1) in if p then 88 else 99")
(equal?! prog-13-3 'int)

(define prog-15-1 "letrec ? f (x : ?) = if zero?(x) then 0 else -((f -(x,1)), -2) in f")
(equal?! prog-15-1 '(int -> int))

(define prog-15-3 "letrec ? even (odd : ?) = proc (x:?) if zero?(x) then 1 else (odd -(x,1)) in 
                   letrec ? odd (x : ?) = if zero?(x) then 0 else ((even odd) -(x,1))
                   in (odd 13)")
(equal?! prog-15-3 'int)

(define prog-pair-1 "unpair a b = newpair(1, zero?(0)) in newpair(a, b)")
(equal?! prog-pair-1 '(int * bool))

(define prog-pair-2 "let f = proc(a : ?) newpair(a, zero?(a)) in (f 1)")
(equal?! prog-pair-2 '(int * bool))

(define prog-let "let a = 1 b = zero?(0) in newpair(a, b)")
(equal?! prog-let '(int * bool))

(define prog-proc "proc(a : ? b : ?) -(a, b)")
(equal?! prog-proc '((int * int) -> int))

(define prog-call "(proc(a : ? b : ?) -(a, b) 1 2)")
(equal?! prog-call 'int)

(define prog-letrec "letrec ? odd(x : ?) = if zero?(x) then 0 else (eve -(x, 1))
                            ? eve(x : ?) = if zero?(x) then 1 else (odd -(x, 1))
                     in newpair(odd, eve)")
(equal?! prog-letrec '((int -> int) * (int -> int)))
