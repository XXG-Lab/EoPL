(define (number->bintree num)
  (list num '() '() '()))

(define (current-element node) (car node))
(define (move-to-left node) (cadr node))
(define (move-to-right node) (caddr node))
(define (move-up node) (cadddr node))

(define (at-leaf? node)
  (or (null? node)
      (and (null? (move-to-left node))
           (null? (move-to-right node)))))

(define (at-root? node)
  (null? (move-up node)))

(define (insert-to-left num node)
  (list (current-element node)
        (list num (move-to-left node) '() node)
        (move-to-right node)))

(define (insert-to-right num node)
  (list (current-element node)
        (move-to-left node)
        (list num '() (move-to-right node) node)))
