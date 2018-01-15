(define (newref val)
  (let ((next-ref (length the-store)))
    (set! the-store (append the-store (list val))) ; `append` is linear
    next-ref))

(define (deref ref)
  (list-ref the-store ref)) ; 'list-ref' is linear

(define (setref! ref val)
  (set! the-store
        (letrec
            ((setref-inner
              (lambda (store1 ref1)
                (cond ((null? store1) (error 'setref! "Invalid reference: " ref the-store))
                      ((zero? ref1) (cons val (cdr store1)))
                      (else (cons (car store1)
                                  (setref-inner (cdr store1)
                                                (- ref1 1))))))))
          (setref-inner the-store ref)))) ; `setref-inner` is linear
