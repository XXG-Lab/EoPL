(require eopl)

(define-datatype bintree bintree?
  (leaf-node
   (num integer?))
  (interior-node
   (key symbol?)
   (left bintree?)
   (right bintree?)))

(define (bintree-to-list node)
  (cases bintree node
    (leaf-node (num) (list 'leaf-node num))
    (interior-node (key left right)
                   (list 'interior-node
                         key
                         (bintree-to-list left)
                         (bintree-to-list right)))))

(equal? (bintree-to-list
         (interior-node 'a (leaf-node 3) (leaf-node 4)))
        '(interior-node
         a
         (leaf-node 3)
         (leaf-node 4)))
