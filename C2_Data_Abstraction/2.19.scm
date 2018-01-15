(define (number->bintree num) (list num '() '()))
(define (current-element node) (car node))
(define (move-to-left node) (cadr node))
(define (move-to-right node) (caddr node))

(define (at-leaf? node)
  (or (null? node)
      (and (null? (move-to-left node))
           (null? (move-to-right node)))))

(define (insert-to-left num node)
  (list (current-element node)
        (list num (move-to-left node) '())
        (move-to-right node)))

(define (insert-to-right num node)
  (list (current-element node)
        (move-to-left node)
        (list num '() (move-to-right node))))

(equal? (number->bintree 13)
        '(13 () ()))
(define t1 (insert-to-right 14
                            (insert-to-left 12
                                            (number->bintree 13))))
(equal? t1 '(13
             (12 () ())
             (14 () ())))
(equal? (move-to-left t1) '(12 () ()))
(equal? (current-element (move-to-left t1)) 12)
(equal? (at-leaf? (move-to-right (move-to-left t1))) #t)
(equal? (insert-to-left 15 t1)
        '(13
          (15
           (12 () ())
           ())
          (14 () ())))
