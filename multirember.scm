;remove all 'a' in list 'lat'
(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) (quote ()))
      (else
       (cond
       ((eq? a (car lat))
        (multirember a (cdr lat)))
       (else
        (cons (car lat)
              (multirember a (cdr lat)))))))))