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
  (lambda (*expr^* *env* *k*)
    (union-case *expr^* expr
                [(const expr1)
                 (let* ([*k-cps* *k*]
                        [*val-apply-k* expr1])
                   (apply-k *k-cps* *val-apply-k*))]
                [(mult x1 x2)
                 (let* (
                        [*k* (kt_make-k-mult-v1 x2 *env* *k*)]
                        [*expr^* x1]
                        [*env* *env*]
                        )
                   (value-of-cps  *expr^* *env* *k*))]
                [(sub1 x)
                 (let* (
                        [*k* (kt_make-k-sub1 *k*)]
                        [*expr^* x]
                        [*env* *env*]
                        )
                   (value-of-cps  *expr^* *env* *k*))]
                [(zero x)
                 (let* (
                        [*k* (kt_make-k-zero *k*)]
                        [*expr^* x]
                        [*env* *env*]
                        )
                   (value-of-cps  *expr^* *env* *k*))]
                [(if test conseq alt)
                 (let* (
                        [*k* (kt_make-k-if conseq alt *env* *k*)]
                        [*expr^* test]
                        [*env* *env*]
                        )
                   (value-of-cps  *expr^* *env* *k*))]
                [(letcc body)
                 (let* (
                        [*k* *k*]
                        [*expr^* body]
                        [*env* (envr_extend-env *env* *k*)]
                        )
                   (value-of-cps  *expr^* *env* *k*))]
                [(throw k-exp v-exp)
                 (let* (
                        [*k* (kt_make-k-throw-k-exp v-exp *env*)]
                        [*expr^* k-exp]
                        [*env* *env*]
                        )
                   (value-of-cps  *expr^* *env* *k*))]
                [(let e body)
                 (let* (
                        [*k* (kt_make-k-let body *env* *k*)]
                        [*expr^* e]
                        [*env* *env*]
                        )
                   (value-of-cps  *expr^* *env* *k*))]
                [(var y)
                 (let* (
                        [*k^* *k*]
                        [*env-cps* *env*]
                        [*val-env* y ]
                        )
                   (apply-env *env-cps* *val-env* *k^*))]
                [(lambda body)
                 (let* (
                        [*k-cps* *k*]
                        [*val-apply-k* (clos_make-closure body *env*)])
                   (apply-k *k-cps* *val-apply-k*))]
                [(app rator rand)
                 (let* (
                        [*k* (kt_make-k-rator rand *env* *k*)]
                        [*expr^* rator]
                        [*env* *env*]
                        )
                   (value-of-cps  *expr^* *env* *k*))])))

(define apply-env
  (lambda (*env-cps* *val-env* *k^*)
    (union-case *env-cps* envr
                [(extend-env env^ val^)
                 (if (zero? *val-env*)
                     (let* ([*k-cps* *k^*]
                            [*val-apply-k* val^])
                       (apply-k *k-cps* *val-apply-k*))
                     (let* (
                            [*k^* *k^*]
                            [*env-cps* env^]
                            [*val-env* (sub1 *val-env*) ]
                            )
                       (apply-env *env-cps* *val-env* *k^*)))]
                [(empty-env) (error 'value-of-cps "unbound identifier")])))

(define apply-closure
  (lambda (*closure-cps* *val-clos* *k-clos*)
    (union-case *closure-cps* clos
                [(make-closure body env)
                 (let* (
                        [*k* *k-clos*]
                        [*expr^* body]
                        [*env* (envr_extend-env env *val-clos*)]
                        )
                   (value-of-cps  *expr^* *env* *k*))])))

(define apply-k
  (lambda (*k-cps* *val-apply-k*)
    (union-case *k-cps* kt
                [(make-k-mult-v2 v1^ k^)
                 (let* ([*k-cps* k^]
                        [*val-apply-k* (* v1^ *val-apply-k*)])
                   (apply-k *k-cps* *val-apply-k*))]
                [(make-k-mult-v1 x2^ env^ k^)
                 (let* (
                        [*k* (kt_make-k-mult-v2 *val-apply-k* k^)]
                        [*expr^* x2^]
                        [*env* env^]
                        )
                   (value-of-cps  *expr^* *env* *k*))]
                [(make-k-sub1 k^)
                 (let* ([*k-cps* k^]
                        [*val-apply-k* (sub1 *val-apply-k*)])
                   (apply-k *k-cps* *val-apply-k*))]
                [(make-k-zero k^)
                 (let* ([*k-cps* k^]
                        [*val-apply-k* (zero? *val-apply-k*)])
                   (apply-k *k-cps* *val-apply-k*))]
                [(make-k-if conseq^ alt^ env^ k^)
                 (let* (
                        [*k* k^]
                        [*expr^* (if *val-apply-k* conseq^ alt^)]
                        [*env* env^]
                        )
                   (value-of-cps  *expr^* *env* *k*))]
                [(make-k-throw-v-exp k^)
                 (let* ([*k-cps* k^]
                        [*val-apply-k* *val-apply-k*])
                   (apply-k *k-cps* *val-apply-k*))]
                [(make-k-throw-k-exp v-exp^ env^)
                 (let* (
                        [*k* (kt_make-k-throw-v-exp *val-apply-k*)]
                        [*expr^* v-exp^]
                        [*env* env^]
                        )
                   (value-of-cps  *expr^* *env* *k*))]
                [(make-k-let body^ env^ k^)
                 (let* (
                        [*k* k^]
                        [*expr^* body^]
                        [*env* (envr_extend-env env^ *val-apply-k*)]
                        )
                   (value-of-cps  *expr^* *env* *k*))]
                [(make-k-rand val-of-rator^ k^)
                 (let* (
                        [*k-clos* k^]
                        [*closure-cps* val-of-rator^]
                        [*val-clos* *val-apply-k*]
                        )
                   (apply-closure *closure-cps* *val-clos* *k-clos*))]
                [(make-k-rator rand^ env^ k^)
                 (let* (
                        [*k* (kt_make-k-rand *val-apply-k* k^)]
                        [*expr^* rand^]
                        [*env* env^]
                        )
                   (value-of-cps  *expr^* *env* *k*))]
                [(empty-k) *val-apply-k*])))

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