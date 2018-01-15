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

(define double-tree
  (lambda (node)
    (if (leaf? node)
        (+ node node)
        (interior-node (contents-of node)
                       (double-tree (lson node))
                       (double-tree (rson node))))))

(equal? (double-tree (list 'a (list 'b 3 (list 'c 4 1)) 2))
        (list 'a (list 'b 6 (list 'c 8 2)) 4))
