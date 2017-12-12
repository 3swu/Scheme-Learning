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
      ((null? tup1) tup2)
      ((null? tup2) tup1)
      (else
       (cons (o+ (car tup1) (car tup2))
             (tup+ (cdr tup1) (cdr tup2)))))))

(define o>
  (lambda (m n)
    (cond
      ((zero? n) #t)
      ((zero? m) #f)
      (else
       (o> (sub1 m) (sub1 n))))))

(define o<
  (lambda (m n)
    (cond
      ((zero? n) #f)
      ((zero? m) #t)
      (else
       (o< (sub1 m) (sub1 n))))))

(define o^
  (lambda (m n)
    (cond
      ((zero? n) 1)
      (else
       (o* m (o^ m (sub1 n)))))))

(define o/
  (lambda (m n)
    (cond
      ((o< m n) 0)
      (else
       (add1 (o/ (o- m n) n))))))

(define pick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (car lat))
      (else
       (pick (sub1 n) (cdr lat))))))

(define rempick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (cdr lat))
      (else
       (cons (car lat) (rempick (sub1 n) (cdr lat)))))))

(define no-nums
  (lambda (lat)
    (cond
      ((null? lat) '())
      (else
       (cond
         ((number? (car lat))
          (no-nums(cdr lat)))
         (else
          (cons (car lat) (no-nums (cdr lat)))))))))

(define occur
  (lambda (n lat)
    (cond
      ((null? lat) 0)
      (else
       (cond
         ((eq? n (car lat))
          (add1 (occur n (cdr lat))))
         (else
          (occur n (cdr lat))))))))