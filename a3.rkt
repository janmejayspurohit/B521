#lang racket
(require racket/trace)

; Q1. lex
(define debruijn
  (λ (v cenv)
    (cond
      [(null? cenv) (error "Invalid")]
      [(eqv? (car cenv) v) 0]
      [else (add1 (debruijn v (cdr cenv)))])))

(define lex
  (λ (e cenv)
    (match e
      [`,y #:when(symbol? y)
           (cond
             [(eqv? (filter (λ (x) (eqv? x y)) cenv) '()) y]
             [else ((λ (var)
                      `(,var ,(debruijn y cenv)))
                    'var)])]
      [`(lambda (,x) ,body) #:when (symbol? x)
                            `(lambda ,(lex body (cons x cenv)))]
      [`(,rator ,rand)
       `(,(lex rator cenv) ,(lex rand cenv))])))

(printf "----------Q1----------\n")

(lex '(lambda (x) x) '())

(lex '(lambda (a)
        (lambda (b)
          (lambda (c)
            (lambda (w)
              (lambda (x)
                (lambda (y)
                  ((lambda (a)
                     (lambda (b)
                       (lambda (c)
                         (((((a b) c) w) x) y))))
                   (lambda (w)
                     (lambda (x)
                       (lambda (y)
                         (((((a b) c) w) x) y))))))))))) 
     '())

(lex '(lambda (a)
        (lambda (b)
          (lambda (c)
            (lambda (w)
              (lambda (x)
                (lambda (y)
                  ((lambda (a)
                     (lambda (b)
                       (lambda (c)
                         (((((a b) c) w) x) y))))
                   (lambda (w)
                     (lambda (x)
                       (lambda (y)
                         (((((a b) c) w) h) y))))))))))) 
     '())


; Q2. value-of

(define value-of
  (λ (e env)
    (match e
      [`,n #:when (number? n) n]
      [`,b #:when (boolean? b) b]
      [`,y #:when (symbol? y) (env y)]
      (`(if ,t ,c ,a) (if (value-of t env) (value-of c env) (value-of a env)))
      [`(zero? ,x) (zero? (value-of x env))]
      [`(sub1 ,x) (sub1 (value-of x env))]
      [`(begin2 ,x ,y) (begin (value-of x env)(value-of y env))]
      [`(set! ,b ,value) (set-box! (box (value-of b env)) (value-of value env))]
      [`(let ([,x ,val]) ,body) (let ([v (value-of val env)])
                                  (value-of body
                                            (λ (arg)
                                              (if (eqv? arg x) v
                                                  (env arg)))))]
      [`(* ,x ,y) (* (value-of x env) (value-of y env))]
      [`(lambda (,x) ,body)
       #:when (symbol? x)
       (λ (arg)
         (value-of body (λ (y)
                          (cond
                            [(eqv? y x) arg]
                            [(number? x) x]
                            [else (env y)]))))]
      [`(,rator ,rand)
       ((value-of rator env) (value-of rand env))])))

(define value-of-fn
  (λ (e env)
    (match e
      [`,n #:when (number? n) n]
      [`,b #:when (boolean? b) b]
      [`,y #:when (symbol? y) (apply-env-fn env y)]
      (`(if ,t ,c ,a) (if (value-of-fn t env) (value-of-fn c env) (value-of-fn a env)))
      [`(zero? ,x) (zero? (value-of-fn x env))]
      [`(* ,x ,y) (* (value-of-fn x env) (value-of-fn y env))]
      [`(sub1 ,x) (sub1 (value-of-fn x env))]
      [`(let ([,x ,val]) ,body) (let ([v (value-of-fn val env)])
                                  (value-of-fn body
                                            (λ (arg)
                                              (if (eqv? arg x) v
                                                  (env arg)))))]
      [`(lambda (,x) ,body)
       #:when (symbol? x)
       (λ (arg)
         (value-of-fn body (extend-env-fn x arg env)))]
      [`(,rator ,rand)
       ((value-of-fn rator env) (value-of-fn rand env))])))

(define apply-env-fn
  (λ (env y)
    (env y)))

(define extend-env-fn
  (λ (x arg env)
    (λ (y)
      (cond
        [(eqv? y x) arg]
        [else (apply-env-fn env y)]))))

(define empty-env-fn
  (lambda ()
    (lambda (y) (error 'value-of-fn "variable not found ~s" y))))


(printf "----------Q2----------\n")
(value-of
 '((lambda (x) (if (zero? x)
                   #t
                   #f))
   1)
 (lambda (y) (error 'value-of "unbound variable ~s" y)))

(value-of 
 '((lambda (x) (if (zero? x)
                   12 
                   47)) 
   10) 
 (lambda (y) (error 'value-of "unbound variable ~s" y)))

(value-of
 '(let ([y (* 3 4)])
    ((lambda (x) (* x y)) (sub1 6)))
 (lambda (y) (error 'value-of "unbound variable ~s" y)))

(value-of
 '(let ([x (* 2 3)])
    (let ([y (sub1 x)])
      (* x y)))
 (lambda (y) (error 'value-of "unbound variable ~s" y)))

(printf "----------Q3----------\n")
(value-of-fn 
 '((lambda (x) (if (zero? x) 
                   12 
                   47)) 
   0) 
 (empty-env-fn))

(value-of-fn
 '(((lambda (f)
      (lambda (n) (if (zero? n) 1 (* n ((f f) (sub1 n))))))
    (lambda (f)
      (lambda (n) (if (zero? n) 1 (* n ((f f) (sub1 n)))))))
   5)
 (empty-env-fn))

(value-of-fn
 '(let ([x (* 2 3)])
    (let ([x (sub1 x)])
      (* x x)))
 (empty-env-fn))

(value-of-fn
 '(let ([x (* 2 3)])
    (let ([y (sub1 x)])
      (* x y)))
 (empty-env-fn))

(printf "----------Q4----------\n")
(value-of
 '(* (begin2 1 1) 3)
 (lambda (y) (error 'value-of "unbound variable ~s" y)))

(value-of
 '((lambda (a)
     ((lambda (p)
        (begin2
          (p a)
          a))
      (lambda (x) (set! x 4))))
   3)
 (lambda (y) (error 'value-of "unbound variable ~s" y)))

(value-of
 '((lambda (f)
     ((lambda (g)
        ((lambda (z) (begin2
                       (g z)
                       z))
         55))
      (lambda (y) (f y)))) (lambda (x) (set! x 44)))
 (lambda (y) (error 'value-of "unbound variable ~s" y)))

(value-of
 '((lambda (x)
     (begin2 (set! x 5) x))
   6)
 (lambda (y) (error 'value-of "unbound variable ~s" y)))

(value-of 
 '(let ((a 3)) 
    (begin2 (begin2 a (set! a 4)) a))
 (lambda (y) (error 'value-of "unbound variable ~s" y)))

(value-of 
 '((lambda (x)
     (begin2
       ((lambda (y)
          (begin2
            (set! x 0)
            98))
        99)
       x))
   97)
 (lambda (y) (error 'value-of "unbound variable ~s" y)))

(value-of 
 '(let ((a 5))
    (let ((y (begin2 (set! a (sub1 a)) 6)))
      (begin2
        (* y y)
        a)))
 (lambda (y) (error 'value-of "unbound variable ~s" y)))

; Q5. value-of-lex

(define value-of-lex
  (lambda (exp env)
    (match exp
      [`(const ,expr) expr]
      [`(mult ,x1 ,x2) (* (value-of-lex x1 env) (value-of-lex x2 env))]
      [`(zero ,x) (zero? (value-of-lex x env))]
      (`(sub1 ,body) (sub1 (value-of-lex body env)))
      (`(if ,t ,c ,a) (if (value-of-lex t env) (value-of-lex c env) (value-of-lex a env)))
      (`(var ,num) (apply-env-lex env num))
      (`(lambda ,body) (lambda (a) (value-of-lex body (extend-env-lex a env))))
      (`(,rator ,rand) ((value-of-lex rator env) (value-of-lex rand env))))))
 
(define empty-env-lex 
  (lambda ()
    (lambda (y) (error 'value-of "unbound variable ~s" y))))

(define apply-env-lex
  (λ (env y)
    (env y)))

(define extend-env-lex
  (λ (arg env)
    (λ (y)
      (cond
        [(zero? y) arg]
        [(eqv? y arg) y]
        [else (apply-env-lex env y)]))))

; Storing env as a list as we get only val and env
; Can we do it this way:
; (define extend-env-lex list)?
; (define apply-env-lex list-ref)

(printf "----------Q5----------\n")
(value-of-lex '((lambda (var 0)) (const 15)) (empty-env-lex))


; Q6. csub1
(define c0 (lambda (f) (lambda (x) x)))
(define c5 (lambda (f) (lambda (x) (f (f (f (f (f x))))))))

(define csub1
  (lambda (f)
    (lambda (m)
      (lambda (b)
        (sub1 ((f m) b))))))

(printf "----------Q6----------\n")
(((csub1 c5) add1) 0)
(((csub1 c0) add1) 0)
