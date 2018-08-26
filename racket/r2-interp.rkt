#lang racket

;;R2语言解释器

;;对环境的基本操作的定义

;空环境
(define env0 '())

;环境拓展，实现变量的绑定
(define ext-env
  (lambda (x v env)
    (cons `(,x . ,v) env)))

;在环境中查找，没找到返回#f
(define lookup
  (lambda (x env)
    (let ([p (assq x env)])
      (cond
        [(not p) #f]
        [else (cdr p)]))))

;定义闭包的数据结构，包含函数定义和它定义时的环境
(struct Closure (f env))

;;解释器的递归定义
(define interp
  (lambda (exp env)
    (match exp
      [(? symbol? x)                        ;变量
       (let ([v (lookup x env)])
         (cond
           [(not v)
            (error "undefined variable" x)]
           [else v]))]
      [(? number? x) x]                     ;数字
      [`(lambda (,x) ,e)                    ;函数
       (Closure exp env)]
      [`(let ([,x ,e1]) ,e2)                ;绑定
       (let ([v1 (interp e1 env)])
         (interp e2 (ext-env x v1 env)))]
      [`(,e1 ,e2)                           ;调用
       (let ([v1 (interp e1 env)]
             [v2 (interp e2 env)])
         (match v1
           [(Closure `(lambda (,x) ,e) env-save)
            (interp e (ext-env x v2 env-save))]))]
      [`(,op ,e1 ,e2)                       ;算术表达式
       (let ([v1 (interp e1 env)]
             [v2 (interp e2 env)])
         (match op
           ['+ (+ v1 v2)]
           ['- (- v1 v2)]
           ['* (* v1 v2)]
           ['/ (/ v1 v2)]))])))

;;用户界面函数
(define r2
  (lambda (exp)
    (interp exp env0)))