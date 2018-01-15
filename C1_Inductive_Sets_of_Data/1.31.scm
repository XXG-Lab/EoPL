(define leaf
  (lambda (num)
    (if (number? num)
        num
        (error 'leaf "Not a number."))))

(define interior-node
  (lambda (sym left right)
    (if (symbol? sym)
        (list sym left right)
        (error 'interior-node "Not a symbol."))))

(define leaf?
  (lambda (node)
    (number? node)))

(define lson
  (lambda (node)
    (cadr node)))

(define rson
  (lambda (node)
    (caddr node)))

(define contents-of
  (lambda (node)
    (if (leaf? node)
        node
        (car node))))
