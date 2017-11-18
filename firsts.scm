;build a new list contains all car of sub-list
(define firsts
  (lambda (lat)
    (cond
      ((null? lat) (quote ()))
      (else
       (cons (car (car lat))
             (firsts (cdr lat)))))))