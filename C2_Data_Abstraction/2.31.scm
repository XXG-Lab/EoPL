(require eopl)

(define-datatype prefix-exp prefix-exp?
  (const-exp
   (num integer?))
  (diff-exp
   (operand1 prefix-exp?)
   (operand2 prefix-exp?)))

(define (parse datum)
  (define (parse-rec datum)
    (if (eqv? '- (car datum))
        (let ((left-ret (parse-rec (cdr datum))))
          (let ((right-ret (parse-rec (cadr left-ret))))
            (list (diff-exp (car left-ret)
                            (car right-ret))
                  (cadr right-ret))))
        (list (const-exp (car datum))
              (cdr datum))))
  (car (parse-rec datum)))

(equal? (parse '(- - 3 2 - 4 - 12 7))
        (diff-exp
         (diff-exp
          (const-exp 3)
          (const-exp 2))
         (diff-exp
          (const-exp 4)
          (diff-exp
           (const-exp 12)
           (const-exp 7)))))
