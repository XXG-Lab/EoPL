(define empty-stack (lambda () '()))
(define push (lambda (s x) (cons x s)))
(define pop (lambda (s) (cdr s)))
(define top (lambda (s) (car s)))
(define empty-stack? (lambda (s) (equal? s '())))
; Constructors: empty-stack, push, pop;
; Observers: top, emtpy-stack?.