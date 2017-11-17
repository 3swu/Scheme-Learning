(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? old (car lat)) (cons
                            (car lat) (cons new (cdr lat))))
      (else (cons (car lat)
                  (insertR new old (cdr lat)))))))