(define remove-first
  (lambda (s los)
    (if (null? los)
        '()
        (if (eqv? (car los) s)
            (cdr los)
            (remove-first s (cdr los))))))

; Remove the elements before and equal to the position that the element first appears.

(remove-first 'a '(b a c))
