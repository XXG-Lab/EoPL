(define empty-stack
  (lambda ()
    (lambda (op)
      (cond ((eqv? op 'pop) (error pop "The stack is empty."))
            ((eqv? op 'empty) #t)
            (else (error pop "The stack is empty."))))))

(define push
  (lambda (s x)
    (lambda (op)
      (cond ((eqv? op 'pop) s)
            ((eqv? op 'empty) #f)
            (else x)))))

(define pop (lambda (s) (s 'pop)))
(define top (lambda (s) (s 'top)))
(define empty-stack? (lambda (s) (s 'empty)))

(equal? (empty-stack? (empty-stack)) #t)
(equal? (top (push (empty-stack) 1)) 1)
(equal? (empty-stack? (push (empty-stack) 1)) #f)
(equal? (top (pop (push (push (empty-stack) 1) 2))) 1)
