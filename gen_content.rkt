#lang racket

(define (deal-file dir-path file-path)
  (let ((file-path-string (path->string file-path)))
    (let ((file-path-strings (string-split file-path-string ".")))
      (let ((file-name (string-append (car file-path-strings)
                                      "."
                                      (cadr file-path-strings))))
        (string-append "["
                       file-name
                       "](./"
                       dir-path
                       "/"
                       file-path-string
                       ")")))))

(define (deal-files-rev dir-path file-list col-num)
  (if (null? file-list)
      (if (equal? col-num 10)
          "\n"
          (string-append "| "
                         (deal-files-rev dir-path file-list (+ col-num 1))))
      (if (equal? col-num 10)
          (string-append "\n"
                         (deal-files-rev dir-path file-list 0))
          (let ((curr-link (deal-file dir-path (car file-list)))
                (next-rev (deal-files-rev dir-path (cdr file-list) (+ col-num 1))))
            (if (equal? col-num 0)
                (string-append curr-link
                               next-rev)
                (string-append " | "
                               curr-link
                               next-rev))))))

(define (deal-files dir-path file-list)
  (string-append "   |   |   |   |   |   |   |   |   |   |   \n"
                 ":-:|:-:|:-:|:-:|:-:|:-:|:-:|:-:|:-:|:-:|:-:\n"
                 (deal-files-rev dir-path file-list 0)
                 "\n"))

(define (deal-chapter path)
  (let ((path-string (path->string path)))
    (if (string=? "C" (substring path-string 0 1))
        (string-append "### Chapter "
                       (string-replace (substring path-string 1)
                                       "_"
                                       " ")
                       "\n\n"
                       (deal-files path-string (directory-list path)))
        "")))

(define (deal-chapters path-list)
  (if (null? path-list)
      ""
      (string-append (deal-chapter (car path-list))
                     (deal-chapters (cdr path-list)))))

(display (deal-chapters (directory-list)))