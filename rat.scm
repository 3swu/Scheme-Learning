(define add-rat
  (lambda (x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y)))))
(define sub-rat
  (lambda (x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y)))))

(define mul-rat
  (lambda (x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y)))))

(define div-rat
  (lambda (x y)
    (make-rat (* (numer x) (denom y))
              (* (numer y) (numer x)))))

(define equal-rat?
  (lambda (x y)
    (= (* (numer x) (denom y))
       (* (denom x) (numer y)))))

(define make-rat
  (lambda (a b)
    (cons a b)))

(define numer
  (lambda (a)
    (car a)))

(define denom
  (lambda (a)
    (cdr a)))

(define print-rat
  (lambda (x)
    (newline)
    (display (numer x))
    (display "/")
    (display (denom x))))
