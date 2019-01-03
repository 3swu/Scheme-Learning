(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (inc n)
  (+ n 1))

(define (cube n)
  (* n n n))

(define (sum-cube a b)
  (sum cube a inc b))