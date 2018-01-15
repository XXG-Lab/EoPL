(require eopl)

(define-datatype red-blue-tree red-blue-tree?
  (leaf-node
   (num integer?))
  (red-node
   (left red-blue-tree?)
   (right red-blue-tree?))
  (blue-node
   (trees (trees? red-blue-tree?))))

(define (trees? pred)
  (lambda (x)
    (or (null? x)
        (and (list? x)
             (pred (car x))
             ((trees? pred) (cdr x))))))

(define (copy-tree-rec node count)
  (cases red-blue-tree node
    (leaf-node (num) (leaf-node count))
    (red-node (left right)
              (red-node (copy-tree-rec left (+ count 1))
                        (copy-tree-rec right (+ count 1))))
    (blue-node (nodes)
               (blue-node (map (lambda (x)
                                 (copy-tree-rec x count))
                               nodes)))))

(define (copy-tree node)
  (copy-tree-rec node 0))

(define root (red-node
              (blue-node
               (list (red-node (leaf-node 0) (leaf-node 0))
                     (blue-node '())
                     (leaf-node 0)))
              (leaf-node 0)))
(copy-tree root)
