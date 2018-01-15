(define (identifier? x)
  (and (symbol? x)
       (not (eqv? x 'lambda))))
