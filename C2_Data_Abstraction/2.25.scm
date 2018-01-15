(require eopl)

(define-datatype bintree bintree?
  (leaf-node
   (num integer?))
  (interior-node
   (key symbol?)
   (left bintree?)
   (right bintree?)))

(define (max-interior-rec node)
  (cases bintree node
    (leaf-node (num) (list '() num num))
    (interior-node (key left right)
                   (let ((left-ret (max-interior-rec left))
                         (right-ret (max-interior-rec right)))
                     (let ((sum (+ (caddr left-ret)
                                   (caddr right-ret))))
                       (if (and (or (null? (car left-ret))
                                    (>= sum (cadr left-ret)))
                                (or (null? (car right-ret))
                                    (>= sum (cadr right-ret))))
                           (list key sum sum)
                           (if (and (not (null? (car left-ret)))
                                    (or (null? (car right-ret))
                                        (>= (cadr left-ret) (cadr right-ret))))
                               (list (car left-ret) (cadr left-ret) sum)
                               (list (car right-ret) (cadr right-ret) sum))))))))

(define (max-interior node)
  (car (max-interior-rec node)))

(define tree-1 (interior-node 'foo (leaf-node 2) (leaf-node 3)))
(define tree-2 (interior-node 'bar (leaf-node -1) tree-1))
(define tree-3 (interior-node 'baz tree-2 (leaf-node 1)))
(equal? (max-interior tree-2) 'foo)
(equal? (max-interior tree-3) 'baz)
