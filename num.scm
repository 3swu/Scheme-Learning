(define add1
  (lambda (m)
    (+ m 1)))

(define sub1
  (lambda (m)
    (- m 1)))

(define o+
  (lambda (m n)
    (cond
      ((zero? n) m)
      (else
       (add1 (o+ m (sub1 n)))))))

(define o-
  (lambda (m n)
    (cond
      ((zero? n) m)
      (else
       (sub1 (o- m (sub1 n)))))))

(define addtup
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else
       (o+ (car tup)
           (addtup (cdr tup)))))))

(define o*
  (lambda (m n)
    (cond
      ((zero? n) 0)
      (else
       (o+ m (o* m (sub1 n)))))))

(define tup+
  (lambda (tup1 tup2)
    (cond
      ((and (null? tup1) (null? tup2))
       '())
      (else
       (cons (o+ (car tup1) (car tup2))
             (tup+ (cdr tup1) (cdr tup2)))))))

