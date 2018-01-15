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

(define-datatype answer answer?
  (an-answer
   (ty type?)
   (subst substitution?)))

(define (type-of-program pgm)
  (cases program pgm
    (a-program (exp1)
               (cases answer (type-of exp1 (empty-tenv) (empty-subst))
                 (an-answer (ty subst)
                            (apply-subst-to-type ty subst))))))

(define (run pgm)
  (type-to-external-form
   (type-of-program
    (scan&parse pgm))))

; BEGIN: Value type
(define (identifier? x)
  (symbol? x))

; BEGIN: Type
(define-datatype type type?
  (tvar-type (serial-number integer?))
  (void-type)
  (int-type)
  (bool-type)
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
    (proc-type (arg-type result-type)
               arg-type)
    (else (error 'proc-type->arg-type ty))))

(define (proc-type->result-type ty)
  (cases type ty
    (proc-type (arg-type result-type)
               result-type)
    (else (error 'proc-type->result-type ty))))

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
    (proc-type (args-type result-type)
               (list (type-to-external-form args-type)
                     '->
                     (type-to-external-form result-type)))))

; BEGIN: Substitution
(define (empty-subst) '())

(define (extend-subst subst tvar ty)
  (cons (cons tvar ty) subst))

(define (substitution? x) (list-of pair?))

(define (apply-one-subst ty0 tvar ty1)
  (cases type ty0
    (void-type () (void-type))
    (int-type () (int-type))
    (bool-type () (bool-type))
    (proc-type (arg-type result-type)
               (proc-type (apply-one-subst arg-type tvar ty1)
                          (apply-one-subst result-type tvar ty1)))
    (tvar-type (sn)
               (if (equal? ty0 tvar) ty1 ty0))))

(define (apply-subst-to-type ty subst)
  (cases type ty
    (void-type () (void-type))
    (int-type () (int-type))
    (bool-type () (bool-type))
    (proc-type (t1 t2)
               (proc-type (apply-subst-to-type t1 subst)
                          (apply-subst-to-type t2 subst)))
    (tvar-type (sn)
               (let ((tmp (assoc ty subst)))
                 (if tmp
                     (apply-subst-to-type (cdr tmp) subst)
                     ty)))))

(define (unifier ty1 ty2 subst exp)
  (let ((ty1 (apply-subst-to-type ty1 subst))
        (ty2 (apply-subst-to-type ty2 subst)))
    (cond ((equal? ty1 ty2) subst)
          ((tvar-type? ty1)
           (if (no-occurrence? ty1 ty2)
               (extend-subst subst ty1 ty2)
               (error 'no-occurrence-violation ty1 ty2 exp)))
          ((tvar-type? ty2)
           (if (no-occurrence? ty2 ty1)
               (extend-subst subst ty2 ty1)
               (error 'no-occurrence-violation ty1 ty2 exp)))
          ((and (proc-type? ty1) (proc-type? ty2))
           (let ((subst (unifier (proc-type->arg-type ty1)
                                 (proc-type->arg-type ty2)
                                 subst exp)))
             (let ((subst (unifier (proc-type->result-type ty1)
                                   (proc-type->result-type ty2)
                                   subst exp)))
               subst)))
          (else (error 'unification-failure ty1 ty2 exp)))))

(define (no-occurrence? tvar ty)
  (cases type ty
    (void-type () #t)
    (int-type () #t)
    (bool-type () #t)
    (proc-type (arg-type result-type)
               (and (no-occurrence? tvar arg-type)
                    (no-occurrence? tvar result-type)))
    (tvar-type (serial-number) (not (equal? tvar ty)))))

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
    (expression ("-" "(" expression "," expression ")") diff-exp)
    (expression ("zero?" "(" expression ")") zero?-exp)
    (expression ("if" expression "then" expression "else" expression) if-exp)
    (expression ("let" identifier "=" expression "in" expression) let-exp)
    (expression ("proc" "(" identifier ":" otype ")" expression) proc-exp)
    (expression ("letrec" otype identifier "(" identifier ":" otype ")" "=" expression
                 "in" expression) letrec-exp)
    (expression ("(" expression expression ")") call-exp)))

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
  (proc-exp (var identifier?)
            (type otype?)
            (body expression?))
  (let-exp (var identifier?)
           (exp expression?)
           (body expression?))
  (letrec-exp (result-type otype?)
              (name identifier?)
              (var identifier?)
              (var-type otype?)
              (exps expression?)
              (body expression?))
  (call-exp (rator expression?)
            (rands expression?)))

; BEGIN: Type-of
(define (type-of exp tenv subst)
  (cases expression exp
    (const-exp (num)
               (an-answer (int-type) subst))
    (var-exp (var)
             (an-answer (apply-tenv tenv var) subst))
    (zero?-exp (exp)
               (cases answer (type-of exp tenv subst)
                 (an-answer (ty1 subst1)
                            (let ((subst2 (unifier ty1 (int-type) subst1 exp)))
                              (an-answer (bool-type) subst2)))))
    (diff-exp (exp1 exp2)
              (cases answer (type-of exp1 tenv subst)
                (an-answer (ty1 subst1)
                           (let ((subst1 (unifier ty1 (int-type) subst1 exp1)))
                             (cases answer (type-of exp2 tenv subst1)
                               (an-answer (ty2 subst2)
                                          (let ((subst2 (unifier ty2 (int-type) subst2 exp2)))
                                            (an-answer (int-type) subst2))))))))
    (if-exp (exp1 exp2 exp3)
            (cases answer (type-of exp1 tenv subst)
              (an-answer (ty1 subst1)
                         (let ((subst1 (unifier ty1 (bool-type) subst1 exp1)))
                           (cases answer (type-of exp2 tenv subst1)
                             (an-answer (ty2 subst2)
                                        (cases answer (type-of exp3 tenv subst2)
                                          (an-answer (ty3 subst3)
                                                     (let ((subst (unifier ty2 ty3 subst3 exp)))
                                                       (an-answer ty2 subst))))))))))
    (let-exp (var exp body)
             (cases answer (type-of exp tenv subst)
               (an-answer (ty subst)
                          (type-of body (extend-tenv var ty tenv) subst))))
    (proc-exp (var otype body)
              (let ((var-type (otype->type otype)))
                (cases answer (type-of body (extend-tenv var var-type tenv) subst)
                  (an-answer (body-type subst)
                             (an-answer (proc-type var-type body-type) subst)))))
    (call-exp (rator rand)
              (let ((result-type (fresh-tvar-type)))
                (cases answer (type-of rator tenv subst)
                  (an-answer (rator-type subst)
                             (cases answer (type-of rand tenv subst)
                               (an-answer (rand-type subst)
                                          (let ((subst (unifier rator-type
                                                                (proc-type rand-type result-type)
                                                                subst exp)))
                                            (an-answer result-type subst))))))))
    (letrec-exp (result-otype name var var-otype exp body)
                (let ((result-type (otype->type result-otype))
                      (var-type (otype->type var-otype)))
                  (let ((body-tenv (extend-tenv name (proc-type var-type result-type) tenv)))
                    (cases answer (type-of exp (extend-tenv var var-type body-tenv) subst)
                      (an-answer (body-type subst)
                                 (let ((subst (unifier body-type result-type subst exp)))
                                   (type-of body body-tenv subst)))))))))

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
