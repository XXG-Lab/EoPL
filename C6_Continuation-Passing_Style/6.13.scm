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
               (initialize-store!)
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
  (list-val
   (lst list?))
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

(define expval->list
  (lambda (val)
    (cases expval val
      (list-val (lst) lst)
      (else (error 'list val)))))

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
    (list-val (lst) (map expval->val lst))
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
    (simple-exp ("-" "(" simple-exp "," simple-exp ")") cps-diff-exp)
    (simple-exp ("zero?" "(" simple-exp ")") cps-zero?-exp)
    (simple-exp ("equal?" "(" simple-exp "," simple-exp ")") cps-equal?-exp)
    (simple-exp ("less?" "(" simple-exp "," simple-exp ")") cps-less?-exp)
    (simple-exp ("greater?" "(" simple-exp "," simple-exp ")") cps-greater?-exp)
    (simple-exp ("proc" "(" (arbno identifier) ")" simple-exp) cps-proc-exp)
    (simple-exp ("cons" "(" simple-exp "," simple-exp ")") cps-cons-exp)
    (simple-exp ("car" "(" simple-exp ")") cps-car-exp)
    (simple-exp ("cdr" "(" simple-exp ")") cps-cdr-exp)
    (simple-exp ("list" "(" (arbno simple-exp) ")") cps-list-exp)
    (simple-exp ("reverse" "(" simple-exp ")") cps-reverse-exp)
    (simple-exp ("null?" "(" simple-exp ")") cps-null?-exp)
    (simple-exp ("emptylist") cps-emptylist-exp)
    (tf-exp (simple-exp) simple-exp->exp)
    (tf-exp ("let" (arbno identifier "=" simple-exp) "in" tf-exp) cps-let-exp)
    (tf-exp ("letrec" (arbno identifier "(" (arbno identifier) ")" "=" tf-exp)
             "in" tf-exp) cps-letrec-exp)
    (tf-exp ("if" simple-exp "then" tf-exp "else" tf-exp) cps-if-exp)
    (tf-exp ("(" simple-exp (arbno simple-exp) ")") cps-call-exp)))

; BEGIN: Expression
(define-datatype simple-exp simple-exp?
  (const-exp (num number?))
  (var-exp (var identifier?))
  (cps-diff-exp (exp1 simple-exp?)
                (exp2 simple-exp?))
  (cps-zero?-exp (exp simple-exp?))
  (cps-equal?-exp (exp1 simple-exp?)
                  (exp2 simple-exp?))
  (cps-less?-exp (exp1 simple-exp?)
                 (exp2 simple-exp?))
  (cps-greater?-exp (exp1 simple-exp?)
                    (exp2 simple-exp?))
  (cps-cons-exp (exp1 simple-exp?)
                (exp2 simple-exp?))
  (cps-car-exp (exp simple-exp?))
  (cps-cdr-exp (exp simple-exp?))
  (cps-list-exp (exps (list-of simple-exp?)))
  (cps-reverse-exp (exp simple-exp?))
  (cps-null?-exp (exp simple-exp?))
  (cps-emptylist-exp)
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
    (cps-let-exp (vars exps body)
                 (let ((vals (map (lambda (simple) (value-of-simple-exp simple env)) exps)))
                   (value-of/k body (extend-env-vals vars vals env) cont)))
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
    (cps-equal?-exp (exp1 exp2)
                    (let ((val1 (value-of-simple-exp exp1 env))
                          (val2 (value-of-simple-exp exp2 env)))
                      (let ((num1 (expval->num val1))
                            (num2 (expval->num val2)))
                        (bool-val
                         (= num1 num2)))))
    (cps-less?-exp (exp1 exp2)
                   (let ((val1 (value-of-simple-exp exp1 env))
                         (val2 (value-of-simple-exp exp2 env)))
                     (let ((num1 (expval->num val1))
                           (num2 (expval->num val2)))
                       (bool-val
                        (< num1 num2)))))
    (cps-greater?-exp (exp1 exp2)
                      (let ((val1 (value-of-simple-exp exp1 env))
                            (val2 (value-of-simple-exp exp2 env)))
                        (let ((num1 (expval->num val1))
                              (num2 (expval->num val2)))
                          (bool-val
                           (> num1 num2)))))
    (cps-cons-exp (exp1 exp2)
                  (let ((val1 (value-of-simple-exp exp1 env))
                        (val2 (value-of-simple-exp exp2 env)))
                    (let ((lst2 (expval->list val2)))
                      (list-val (cons val1 lst2)))))
    (cps-car-exp (exp1)
                 (let ((val1 (value-of-simple-exp exp1 env)))
                   (let ((lst1 (expval->list val1)))
                     (car lst1))))
    (cps-cdr-exp (exp1)
                 (let ((val1 (value-of-simple-exp exp1 env)))
                   (let ((lst1 (expval->list val1)))
                     (list-val (cdr lst1)))))
    (cps-null?-exp (exp1)
                   (let ((val1 (value-of-simple-exp exp1 env)))
                     (let ((lst1 (expval->list val1)))
                       (bool-val (null? lst1)))))
    (cps-emptylist-exp () (list-val '()))
    (cps-list-exp (exps)
                  (list-val (map (lambda (exp) (value-of-simple-exp exp env)) exps)))
    (cps-reverse-exp (exp)
                     (list-val (reverse (expval->list (value-of-simple-exp exp env)))))
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

; BEGIN: Test
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

; removeall
(define program-removeall-1 "
letrec removeall(n lst fst) =
    if null?(lst)
    then reverse(fst)
    else if equal?(n, car(lst))
         then (removeall n cdr(lst) fst)
         else (removeall n cdr(lst) cons(car(lst), fst))
in (removeall 4 list(1 2 3 4) emptylist)
")
(equal?! program-removeall-1 '(1 2 3))

(define program-removeall-2 "
letrec removeall(n lst fst) =
    if null?(lst)
    then reverse(fst)
    else if equal?(n, car(lst))
         then (removeall n cdr(lst) fst)
         else (removeall n cdr(lst) cons(car(lst), fst))
in (removeall 4 list(4 4) emptylist)
")
(equal?! program-removeall-2 '())

(define program-removeall-3 "
letrec removeall(n lst fst) =
    if null?(lst)
    then reverse(fst)
    else if equal?(n, car(lst))
         then (removeall n cdr(lst) fst)
         else (removeall n cdr(lst) cons(car(lst), fst))
in (removeall 4 list(4 3) emptylist)
")
(equal?! program-removeall-3 '(3))

(define program-removeall-4 "
letrec removeall(n lst fst) =
    if null?(lst)
    then reverse(fst)
    else if equal?(n, car(lst))
         then (removeall n cdr(lst) fst)
         else (removeall n cdr(lst) cons(car(lst), fst))
in (removeall 4 list(24 3) emptylist)
")
(equal?! program-removeall-4 '(24 3))

; occurs-in?
(define program-occurs-in?-1 "
letrec occursin(n lst) =
    if null?(lst)
       then 0
       else if equal?(n, car(lst))
            then 1
            else (occursin n cdr(lst))
in (occursin 1 list(1 2 3))
")
(equal?! program-occurs-in?-1 1)

(define program-occurs-in?-2 "
letrec occursin(n lst) =
    if null?(lst)
       then 0
       else if equal?(n, car(lst))
            then 1
            else (occursin n cdr(lst))
in (occursin 1 list(2 2 3))
")
(equal?! program-occurs-in?-2 0)

; Skip