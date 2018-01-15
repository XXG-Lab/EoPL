(define one (lambda () '(one)))
(define diff (lambda (l r) (list 'diff l r)))
(define one? (lambda (t) (eqv? 'one (car t))))
(define diff? (lambda (t) (eqv? 'diff (car t))))
(define left (lambda (t) (cadr t)))
(define right (lambda (t) (caddr t)))

(define zero
  (lambda ()
    (diff (one) (one))))

(define get-diff
  (lambda (t)
    (if (one? t)
        1
        (- (get-diff (left t)) (get-diff (right t))))))

(define is-zero?
  (lambda (t)
    (equal? (get-diff t) 0)))

(define successor
  (lambda (n)
    (diff n (diff (zero) (one)))))

(define predecessor
  (lambda (n)
    (diff n (diff (one) (zero)))))

(define diff-tree-plus
  (lambda (a b)
    (diff (diff a (zero))
          (diff (zero) b))))

(define a (successor (successor (zero))))
(define b (predecessor (zero)))
(equal? (get-diff a) 2)
(equal? (get-diff b) -1)
(define c (diff-tree-plus a b))
(equal? (get-diff c) 1)
