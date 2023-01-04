#lang racket
(require racket/trace)

(define value-of-ds
  (λ (e env)
    (match e
      [`,n #:when (number? n) n]
      [`,b #:when (boolean? b) b]
      [`,y #:when (symbol? y) (apply-env-ds env y)]
      [`(if ,condt ,texp ,fexp) (apply-if condt texp fexp env)]
      [`(zero? ,x) (apply-zero x env)]
      [`(* ,x ,y) (* (value-of-ds x env) (value-of-ds y env))]
      [`(sub1 ,n) #:when (or (number? n) (symbol? n)) (sub1 (value-of-ds n env))]
      [`(let ([,x ,val]) ,body) (apply-let val env body x) ]
      [`(lambda (,x) ,body) #:when (symbol? x) (make-clos body x env)]
      [`(,rator ,rand) (apply-clos (value-of-ds rator env) (value-of-ds rand env))])))

(define make-clos
  (lambda (body x env)
    `(make-clos ,body ,x ,env)))

(define apply-clos
  (lambda (rator rand)
    (match rator
      [`(make-clos ,body ,x ,env)
       (value-of-ds body (extend-env-ds x rand env))])))

(define apply-let
  (lambda (val env body x)
    (let ([v (value-of-ds val env)])
      (value-of-ds body
                   (λ (arg)
                     (if (eqv? arg x) v
                         (env arg)))))))

(define apply-zero
  (lambda (x env)
    (cond
      [(eqv? (value-of-ds x env) 0) #t]
      [else #f])))

(define apply-if
  (lambda (condt texp fexp env)
    (cond
      [(eqv? (value-of-ds condt env) #t)
       (value-of-ds texp env)]
      [else (value-of-ds fexp env)])))

(define apply-env-ds
  (lambda (env y)
    (match env
      [`(extend-env-ds ,x ,arg ,env^)
       (cond
         [(eqv? y x) arg]
         [else (apply-env-ds env^ y)])]
      [else (env y)])
    ))

(define extend-env-ds
  (lambda (x arg env)
    (lambda (y)
      (cond
        [(eqv? y x) arg]
        [else (apply-env-ds env y)]))))

(define empty-env-ds
  (lambda ()
    (lambda (y) `(error "variable not found ~s" y))))


(printf "----------Q1----------\n")

(value-of-ds
 '(((lambda (f)
      (lambda (n) (if (zero? n) 1 (* n ((f f) (sub1 n))))))
    (lambda (f)
      (lambda (n) (if (zero? n) 1 (* n ((f f) (sub1 n)))))))
   5)
 (empty-env-ds))

(value-of-ds
 '(let ([x (* 2 3)])
    (let ([x (sub1 x)])
      (* x x)))
 (empty-env-ds))

(value-of-ds
 '(let ([x (* 2 3)])
    (let ([y (sub1 x)])
      (* x y)))
 (empty-env-ds))

(value-of-ds
 '(((lambda (f)
      (lambda (n)
        (if (zero? n) 1 (* n ((f f) (sub1 n))))))
    (lambda (f)
      (lambda (n)
        (if (zero? n) 1 (* n ((f f) (sub1 n)))))))
   5)
 (empty-env-ds))
