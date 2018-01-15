(require eopl)

(define (list-index pred lst)
  (define (list-index-rec lst idx val)
    (if (null? lst)
        val
        (if (pred (car lst))
            (list-index-rec (cdr lst) (+ idx 1) idx)
            (list-index-rec (cdr lst) (+ idx 1) val))))
  (list-index-rec lst 0 #f))

(equal? (list-index odd? '(1 2 3)) 2)
(equal? (list-index odd? '(2 4 6)) #f)
