(define atom?
  (lambda (a)
    (and (not (pair? a)) (not (null? a)))))

(define rember*
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((atom? (car lat))
       (cond
         ((eq? (car lat) a)
          (rember* a (cdr lat)))
         (else
          (cons (car lat)
                (rember* a (cdr lat))))))
       (else
        (cons (rember* a (car lat))
              (rember* a (cdr lat)))))))