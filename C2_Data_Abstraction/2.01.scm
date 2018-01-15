(define zero
  (lambda (base)
    (list base)))

(define is-zero?
  (lambda (n)
    (or (equal? (cdr n) '(0))
        (equal? (cdr n) '()))))

(define successor-rev
  (lambda (n base)
    (if (null? n)
        '(1)
        (let ((inc (+ (car n) 1)))
          (if (= inc base)
              (cons 0 (successor-rev (cdr n) base))
              (cons inc (cdr n)))))))

(define successor
  (lambda (n)
    (cons (car n) (successor-rev (cdr n) (car n)))))

(define predecessor-rev
  (lambda (n base)
    (let ((dec (- (car n) 1)))
      (if (= dec -1)
          (cons (- base 1) (predecessor-rev (cdr n) base))
          (if (and (= dec 0) (null? (cdr n)))
              '()
              (cons dec (cdr n)))))))

(define predecessor
  (lambda (n)
    (cons (car n) (predecessor-rev (cdr n) (car n)))))

(define add
  (lambda (a b)
    (if (is-zero? b)
        a
        (add (successor a) (predecessor b)))))

(define multiply
  (lambda (a b)
    (if (is-zero? b)
        (zero (car a))
        (add a (multiply a (predecessor b))))))

(define factorial
  (lambda (n)
    (if (is-zero? n)
        (successor (zero (car n)))
        (multiply n (factorial (predecessor n))))))

(define result (factorial '(10 10)))
(display result)
(equal? result '(10 0 0 8 8 2 6 3))
