#lang racket

(define calc
  (lambda (exp)
    (match exp            ;匹配数字与表达式
      [(? number? x) x]
      [`(,op ,e1 ,e2)
       (let ([v1 (calc e1)];递归计算子表达式
             [v2 (calc e2)])
         (match op         ;匹配操作符
           ['+ (+ v1 v2)]
           ['- (- v1 v2)]
           ['* (* v1 v2)]
           ['/ (/ v1 v2)]))])))