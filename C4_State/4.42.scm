(let-exp (var exp body)
         (let ((ref (value-of-operand exp env)))
           (value-of body (extend-env var ref env))))