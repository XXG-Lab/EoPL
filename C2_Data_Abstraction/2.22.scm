(require eopl)

(define-datatype stack stack?
  (empty-stack)
  (push (top (lambda (x) #t))
        (old-stack stack?)))

(define (empty-stack? stk)
  (cases stack stk
    (empty-stack () #t)
    (push (top old-stack) #f)))

(define (pop stk)
  (cases stack stk
    (empty-stack () (eopl:error 'pop "Empty stack."))
    (push (top old-stack) old-stack)))

(define (top stk)
  (cases stack stk
    (empty-stack () (eopl:error top "Empty stack."))
    (push (top old-stack) top)))

(equal? (empty-stack? (empty-stack)) #t)
(equal? (top (push 1 (empty-stack))) 1)
(equal? (top (pop (push 2 (push 1 (empty-stack))))) 1)
