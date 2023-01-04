#lang racket
(require racket/trace)
(require rackunit)

; Q3.
(printf "--------------Q3--------------\n")
(define value-of-cps
  (lambda (expr env k)
    (match expr
      [`(const ,expr) (apply-k k expr)]
      [`(mult ,x1 ,x2)
       (value-of-cps x1 env
                     (λ (val-of-x1)
                       (value-of-cps x2 env
                                     (λ (val-of-x2)
                                       (apply-k k (* val-of-x1 val-of-x2))))))]
      [`(sub1 ,x)
       (value-of-cps x env (λ (val-of-x)
                             (k (sub1 val-of-x))))]
      [`(zero ,x)
       (value-of-cps x env (λ (val-of-x)
                             (k (zero? val-of-x))))]
      [`(if ,test ,conseq ,alt)
       (value-of-cps test env
                     (λ (val-of-test)
                       (if val-of-test
                           (value-of-cps conseq env k)
                           (value-of-cps alt env k))))]
      [`(letcc ,body)
       (value-of-cps body
                     (lambda (y)
                       (if (zero? y) k (env (sub1 y))))
                     (λ (val-of-letcc)
                       (let/cc k val-of-letcc)))]
      [`(throw ,k-exp ,v-exp)
       (value-of-cps v-exp env
                     (λ (val-of-v-exp)
                       (value-of-cps k-exp env
                                     (λ (val-of-k-exp)
                                       (val-of-k-exp val-of-v-exp)))))]
      [`(let [,e ,exp] ,body)
       (value-of-cps body (extend-env env (value-of-cps exp env k)) k)]
      #;(λ (a)
         (value-of-cps body (lambda (y)
                              (if (zero? y) a (env (sub1 y)))) k)
         (value-of-cps e env k))
      [`(var ,y) (env y)]
      [`(lambda ,body)
       (lambda (a)
         (value-of-cps body (lambda (y)
                              (if (zero? y) a (env (sub1 y)))) k))]
      [`(app ,rator ,rand)
       (value-of-cps rand env
                     (λ (val-of-rand)
                       (value-of-cps rator env
                                     (λ (val-of-rator)
                                       (val-of-rator val-of-rand)))))])))
 
(define empty-env
  (λ ()
    `(empty-env)))
 
(define empty-k
  (λ ()
    `(empty-k)))

(define apply-k
  (λ (k v)
    (match k
      [`(empty-k) v]
      [else (k v)])))

(define extend-env
  (λ (env v)
    `(extend-env ,env ,v)))

(define apply-closure
  (λ (closure v k)
    (match closure
      [`(apply-closure ,body ,env) (value-of-cps body (extend-env env v) k)])))

(define apply-env
  (λ (env v k)
    (match env
      [`(empty-env) (error `value-of-cps "unbound identifier")]
      [`(extend-env ,env ,val) (if (zero? v)
                                   (apply-k k val)
                                   (apply-env env (sub1 v) k))])))

#;(define check-equal?
    (λ (x y)
      (if (eqv? x y)
          (printf "Match!\n")
          (printf "Fail!\n"))))

(check-equal? (value-of-cps '(const 5) (empty-env) (empty-k)) 5)

(check-equal? (value-of-cps '(mult (const 5) (const 5)) (empty-env) (empty-k)) 25)

(check-equal? (value-of-cps '(sub1 (sub1 (const 5))) (empty-env) (empty-k)) 3)

(check-equal? (value-of-cps '(if (zero (const 0)) (mult (const 2) (const 2)) (const 3)) (empty-env) (empty-k)) 4)

(check-equal? (value-of-cps '(app (app (lambda (lambda (var 1))) (const 6)) (const 5)) (empty-env) (empty-k)) 6)

(check-equal? (value-of-cps '(app (lambda (app (lambda (var 1)) (const 6))) (const 5)) (empty-env) (empty-k)) 5)

(check-equal? (value-of-cps '(let (const 6) (const 4)) (empty-env) (empty-k)) 4)

(check-equal? (value-of-cps '(let (const 5) (var 0)) (empty-env) (empty-k)) 5)

(check-equal? (value-of-cps '(mult (const 5) (let (const 5) (var 0))) (empty-env) (empty-k)) 25)

(check-equal? (value-of-cps '(app (if (zero (const 4)) (lambda (var 0)) (lambda (const 5))) (const 3)) (empty-env) (empty-k)) 5)

(check-equal? (value-of-cps '(app (if (zero (const 0)) (lambda (var 0)) (lambda (const 5))) (const 3)) (empty-env) (empty-k)) 3)

(check-equal? (value-of-cps '(letcc (throw (throw (var 0) (const 5)) (const 6))) (empty-env) (empty-k)) 5)

(check-equal? (value-of-cps '(letcc (throw (const 5) (throw (var 0) (const 5)))) (empty-env) (empty-k)) 5)

(check-equal? (value-of-cps '(mult (const 3) (letcc (throw (const 5) (throw (var 0) (const 5))))) (empty-env) (empty-k)) 15)

(check-equal? (value-of-cps '(if (zero (const 5)) (app (lambda (app (var 0) (var 0))) (lambda (app (var 0) (var 0)))) (const 4))
                            (empty-env)
                            (empty-k))
              4)

(check-equal? (value-of-cps '(if (zero (const 0)) (const 4) (app (lambda (app (var 0) (var 0))) (lambda (app (var 0) (var 0)))))
                            (empty-env)
                            (empty-k))
              4)

(check-equal? (value-of-cps '(app (lambda (app (app (var 0) (var 0)) (const 2)))
                                  (lambda
                                      (lambda 
                                          (if (zero (var 0))  
                                              (const 1)
                                              (app (app (var 1) (var 1)) (sub1 (var 0)))))))
                            (empty-env)
                            (empty-k))
              1)

