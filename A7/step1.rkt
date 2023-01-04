#lang racket
(require rackunit)
(require racket/trace)

(define value-of-cps
  (lambda (expr env k)
    (match expr
      [`(const ,expr) (k expr)]
      [`(mult ,x1 ,x2)
       (value-of-cps x1 env
                     (lambda (v1)
                       (value-of-cps x2 env
                                     (lambda (v2)
                                       (k (* v1 v2))))))]
      [`(sub1 ,x)
       (value-of-cps x env
                     (lambda (v)
                       (k (sub1 v))))]
      [`(zero ,x)
       (value-of-cps x env
                     (lambda (v)
                       (k (zero? v))))]
      [`(if ,test ,conseq ,alt)
       (value-of-cps test env
                     (lambda (v)
                       (if v
                           (value-of-cps conseq env k)
                           (value-of-cps alt env k))))]
      [`(letcc ,body)
       (value-of-cps body (lambda (y k^)
                            (if (zero? y)
                                (k^ k)
                                (env (sub1 y)))) k)
       ]
      [`(throw ,k-exp ,v-exp)
       (value-of-cps k-exp env
                     (lambda (k)
                       (value-of-cps v-exp env
                                     (lambda (v)
                                       (k v)))))]
      [`(let ,e ,body)
       (value-of-cps e env
                     (lambda (v)
                       (value-of-cps body
                                     (lambda (y k^)
                                       (if (zero? y)
                                           (k^ v)
                                           (env (sub1 y))))
                                     k)))]
      [`(var ,y) (env y k)]
      [`(lambda ,body)
       (k (lambda (a k^)
            (value-of-cps
             body
             (lambda (y k^^)
               (if (zero? y)
                   (k^^ a)
                   (env (sub1 y) k^^)))
             k^)))]
      [`(app ,rator ,rand)
       (value-of-cps rator env
                     (lambda (val-of-rator)
                       (value-of-cps rand env
                                     (lambda (val-of-rand)
                                       (val-of-rator val-of-rand k)))
                       ))])))

;;; (trace value-of-cps)

(define empty-env
  (lambda ()
    (lambda (y k)
      (error 'value-of-cps "unbound identifier"))))

(define empty-k
  (lambda ()
    (lambda (v)
      v)))

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