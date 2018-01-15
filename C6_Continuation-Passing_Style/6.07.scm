(define value-of/k
  (lambda (exp env cont)
    (cases expression exp
      (const-exp (num) (cont (num-val num)))
      (var-exp (var) (apply-cont cont (apply-env env var)))
      (proc-exp (var body)
                (cont (proc-val
                       (procedure var body env))))
      (letrec-exp (p-name b-var p-body letrec-body)
                  (value-of/k letrec-body (extend-env-rec p-name b-var p-body env) cont))
      (zero?-exp (exp1)
                 (value-of/k exp1 env
                             (lambda (val)
                               (cont (bool-val (zero? (expval->num val)))))))
      (if-exp (exp1 exp2 exp3)
              (value-of/k exp1 env
                          (lambda (val)
                            (if (expval->bool val)
                                (value-of/k exp2 env cont)
                                (value-of/k exp3 env cont)))))
      (let-exp (var exp1 body)
               (value-of/k exp1 env
                           (lambda (val)
                             (value-of/k body (extend-env var val env) cont))))
      (diff-exp (exp1 exp2)
                (value-of/k exp1 env
                            (lambda (val1)
                              (value-of/k exp2 env
                                          (lambda (val2)
                                            (let ((num1 (expval->num val1))
                                                  (num2 (expval->num val2)))
                                              (cond (num->val (+ num1 num2)))))))))
      (call-exp (rator rand)
                (value-of/k rator env
                            (lambda (val1)
                              (value-of/k rand env
                                          (lambda (val2)
                                            (let ((proc (expval->proc val1)))
                                              (apply-procedure/k proc1 val2 cont))))))))))

(define apply-procedure/k
  (lambda (proc1 val cont)
    (cases proc proc1
      (procedure (var body saved-env)
                 (value-of/k body (extend-env var val saved-env) cont)))))
