(define value-of-operand
  (lambda (exp env)
    (cases (exp env)
      (var-exp (var) (apply-env env var))
      (const-exp (num) (newref (num-val num)))
      (proc-exp (var body) (newref (value-of exp env)))
      (else (newref (a-thunk exp env))))))