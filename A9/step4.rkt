#lang racket
(require "parenthec.rkt")

(define-union expr
  (const cexp)
  (var n)
  (if test conseq alt)
  (mult nexp1 nexp2)
  (sub1 nexp)
  (zero nexp)
  (letcc body)
  (throw kexp vexp)
  (let exp body)
  (lambda body)
  (app rator rand))

(define-union clos
  (make-closure body env))

(define-union envr
  (extend-env env^ val^)
  (empty-env))

(define-union kt
  (make-k-mult-v2 v1^ k^)
  (make-k-mult-v1 x2^ env^ k^)
  (make-k-sub1 k^) 
  (make-k-zero k^)
  (make-k-if conseq^ alt^ env^ k^)
  (make-k-throw-v-exp k^)
  (make-k-throw-k-exp v-exp^ env^)
  (make-k-let body^ env^ k^)
  (make-k-rand val-of-rator^ k^)
  (make-k-rator rand^ env^ k^)
  (empty-k))

(define value-of-cps
  (lambda (expr^ env k)
    (union-case expr^ expr
                [(const expr1) (apply-k k expr1)]
                [(mult x1 x2)
                 (value-of-cps x1 env (kt_make-k-mult-v1 x2 env k))]
                [(sub1 x)
                 (value-of-cps x env (kt_make-k-sub1 k))]
                [(zero x)
                 (value-of-cps x env (kt_make-k-zero k))]
                [(if test conseq alt)
                 (value-of-cps test env (kt_make-k-if conseq alt env k))]
                [(letcc body)
                 (value-of-cps body (envr_extend-env env k) k)]
                [(throw k-exp v-exp)
                 (value-of-cps k-exp env (kt_make-k-throw-k-exp v-exp env))]
                [(let e body)
                 (value-of-cps e env (kt_make-k-let body env k))]
                [(var y) (apply-env env y k)]
                [(lambda body)
                 (apply-k k (clos_make-closure body env))]
                [(app rator rand)
                 (value-of-cps rator env (kt_make-k-rator rand env k))])))

(define empty-env
  (lambda ()
    `(empty-env)))

(define empty-k
  (lambda ()
    `(empty-k)))

(define apply-env
  (lambda (env-cps val k^)
    (union-case env-cps envr
                [(extend-env env^ val^)
                 (if (zero? val)
                     (apply-k k^ val^)
                     (apply-env env^ (sub1 val) k^))]
                [(empty-env) (error 'value-of-cps "unbound identifier")]
                )))

(define apply-closure
  (lambda (closure-cps val k)
    (union-case closure-cps clos
                [(make-closure body env)
                 (value-of-cps body (envr_extend-env env val) k)])))

(define apply-k
  (lambda (k-cps val)
    (union-case k-cps kt
                [(make-k-mult-v2 v1^ k^)
                 (apply-k k^ (* v1^ val))]
                [(make-k-mult-v1 x2^ env^ k^)
                 (value-of-cps x2^ env^ (kt_make-k-mult-v2 val k^))]
                [(make-k-sub1 k^) (apply-k k^ (sub1 val))]
                [(make-k-zero k^) (apply-k k^ (zero? val))]
                [(make-k-if conseq^ alt^ env^ k^)
                 (value-of-cps (if val conseq^ alt^) env^ k^)]
                [(make-k-throw-v-exp k^) (apply-k k^ val)]
                [(make-k-throw-k-exp v-exp^ env^) (value-of-cps v-exp^ env^ (kt_make-k-throw-v-exp val))]
                [(make-k-let body^ env^ k^)
                 (value-of-cps body^ (envr_extend-env env^ val) k^)]
                [(make-k-rand val-of-rator^ k^) (apply-closure val-of-rator^ val k^)]
                [(make-k-rator rand^ env^ k^) (value-of-cps rand^ env^ (kt_make-k-rand val k^))]
                [(empty-k) val]
                )))

(define extend-env
  (lambda (env^ val^)
    `(extend-env ,env^ ,val^)))

(define make-closure
  (lambda (body env)
    `(make-closure ,body ,env)))

(define make-k-mult-v2
  (lambda (v1^ k^)
    `(make-k-mult-v2 ,v1^ ,k^)))

(define make-k-mult-v1
  (lambda (x2^ env^ k^)
    `(make-k-mult-v1 ,x2^ ,env^ ,k^)))

(define make-k-sub1
  (lambda (k^)
    `(make-k-sub1 ,k^)))

(define make-k-zero
  (lambda (k^)
    `(make-k-zero ,k^)))

(define make-k-if
  (lambda (conseq^ alt^ env^ k^)
    `(make-k-if ,conseq^ ,alt^ ,env^ ,k^)))

(define make-k-throw-v-exp
  (lambda (k^)
    `(make-k-throw-v-exp ,k^)))

(define make-k-throw-k-exp
  (lambda (v-exp^ env^)
    `(make-k-throw-k-exp ,v-exp^ ,env^)))

(define make-k-let
  (lambda (body^ env^ k^)
    `(make-k-let ,body^ ,env^ ,k^)))

(define make-k-rand
  (lambda (val-of-rator^ k^)
    `(make-k-rand ,val-of-rator^ ,k^)))

(define make-k-rator
  (lambda (rand^ env^ k^)
    `(make-k-rator ,rand^ ,env^ ,k^)))

(define main
  (lambda ()
    (value-of-cps
     (expr_let
      (expr_lambda
       (expr_lambda
        (expr_if
         (expr_zero (expr_var 0))
         (expr_const 1)
         (expr_mult (expr_var 0) (expr_app (expr_app (expr_var 1) (expr_var 1)) (expr_sub1 (expr_var 0)))))))
      (expr_mult
       (expr_letcc
        (expr_app
         (expr_app (expr_var 1) (expr_var 1))
         (expr_throw (expr_var 0) (expr_app (expr_app (expr_var 1) (expr_var 1)) (expr_const 4)))))
       (expr_const 5)))
     (envr_empty-env)
     (kt_empty-k))))

(main)