(define nth-element-rec
  (lambda (o-lst o-n lst n)
    (if (null? lst)
        (report-list-too-short o-lst o-n)
        (if (zero? n)
            (car lst)
            (nth-element-rec o-lst o-n (cdr lst) (- n 1))))))

(define nth-element
  (lambda (lst n)
    (nth-element-rec lst n lst n)))

(define report-list-too-short
  (lambda (lst n)
    (error 'nth-element
           "~s does not have ~s elements.~%" lst (+ n 1))))

(nth-element '(a b c d e) 4)
(nth-element '(a b c d e) 5)
