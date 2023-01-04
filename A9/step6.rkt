#lang racket
(require "parenthec.rkt")

(define-registers
  *closure-cps*
  *val-clos*
  *k-clos*
  *env-cps*
  *val-env*
  *k^*
  *k-cps*
  *val-apply-k*
  *expr^*
  *env*
  *k*)

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
  (lambda () ;*expr^* *env* *k*
    (union-case *expr^* expr
                [(const expr1)
                 (begin [set! *k-cps* *k*]
                        [set! *val-apply-k* expr1]
                        (apply-k))]
                [(mult x1 x2)
                 (begin
                   [set! *k* (kt_make-k-mult-v1 x2 *env* *k*)]
                   [set! *expr^* x1]
                   [set! *env* *env*]
                   (value-of-cps))]
                [(sub1 x)
                 (begin
                   [set! *k* (kt_make-k-sub1 *k*)]
                   [set! *expr^* x]
                   [set! *env* *env*]
                   (value-of-cps))]
                [(zero x)
                 (begin
                   [set! *k* (kt_make-k-zero *k*)]
                   [set! *expr^* x]
                   [set! *env* *env*]
                   (value-of-cps))]
                [(if test conseq alt)
                 (begin
                   [set! *k* (kt_make-k-if conseq alt *env* *k*)]
                   [set! *expr^* test]
                   [set! *env* *env*]
                   (value-of-cps))]
                [(letcc body)
                 (begin
                   [set! *k* *k*]
                   [set! *expr^* body]
                   [set! *env* (envr_extend-env *env* *k*)]
                   (value-of-cps))]
                [(throw k-exp v-exp)
                 (begin
                   [set! *k* (kt_make-k-throw-k-exp v-exp *env*)]
                   [set! *expr^* k-exp]
                   [set! *env* *env*]
                   (value-of-cps))]
                [(let e body)
                 (begin
                   [set! *k* (kt_make-k-let body *env* *k*)]
                   [set! *expr^* e]
                   [set! *env* *env*]
                   (value-of-cps))]
                [(var y)
                 (begin
                   [set! *k^* *k*]
                   [set! *env-cps* *env*]
                   [set! *val-env* y ]
                   (apply-env))]
                [(lambda body)
                 (begin [set! *k-cps* *k*]
                        [set! *val-apply-k* (clos_make-closure body *env*)]
                        (apply-k))]
                [(app rator rand)
                 (begin
                   [set! *k* (kt_make-k-rator rand *env* *k*)]
                   [set! *expr^* rator]
                   [set! *env* *env*]
                   (value-of-cps))])))

(define apply-env
  (lambda () ;*env-cps* *val-env* *k^*
    (union-case *env-cps* envr
                [(extend-env env^ val^)
                 (if (zero? *val-env*)
                     (begin [set! *k-cps* *k^*]
                            [set! *val-apply-k* val^]
                            (apply-k))
                     (begin
                       [set! *k^* *k^*]
                       [set! *env-cps* env^]
                       [set! *val-env* (sub1 *val-env*) ]
                       (apply-env)))]
                [(empty-env) (error 'value-of-cps "unbound identifier")])))

(define apply-closure
  (lambda () ;*closure-cps* *val-clos* *k-clos*
    (union-case *closure-cps* clos
                [(make-closure body env)
                 (begin
                   [set! *k* *k-clos*]
                   [set! *expr^* body]
                   [set! *env* (envr_extend-env env *val-clos*)]
                   (value-of-cps))])))

(define apply-k
  (lambda () ;*k-cps* *val-apply-k*
    (union-case *k-cps* kt
                [(make-k-mult-v2 v1^ k^)
                 (begin [set! *k-cps* k^]
                        [set! *val-apply-k* (* v1^ *val-apply-k*)]
                        (apply-k))]
                [(make-k-mult-v1 x2^ env^ k^)
                 (begin
                   [set! *k* (kt_make-k-mult-v2 *val-apply-k* k^)]
                   [set! *expr^* x2^]
                   [set! *env* env^]
                   (value-of-cps))]
                [(make-k-sub1 k^)
                 (begin [set! *k-cps* k^]
                        [set! *val-apply-k* (sub1 *val-apply-k*)]
                        (apply-k))]
                [(make-k-zero k^)
                 (begin [set! *k-cps* k^]
                        [set! *val-apply-k* (zero? *val-apply-k*)]
                        (apply-k))]
                [(make-k-if conseq^ alt^ env^ k^)
                 (begin
                   [set! *k* k^]
                   [set! *expr^* (if *val-apply-k* conseq^ alt^)]
                   [set! *env* env^]
                   (value-of-cps))]
                [(make-k-throw-v-exp k^)
                 (begin [set! *k-cps* k^]
                        [set! *val-apply-k* *val-apply-k*]
                        (apply-k))]
                [(make-k-throw-k-exp v-exp^ env^)
                 (begin
                   [set! *k* (kt_make-k-throw-v-exp *val-apply-k*)]
                   [set! *expr^* v-exp^]
                   [set! *env* env^]
                   (value-of-cps))]
                [(make-k-let body^ env^ k^)
                 (begin
                   [set! *k* k^]
                   [set! *expr^* body^]
                   [set! *env* (envr_extend-env env^ *val-apply-k*)]
                   (value-of-cps))]
                [(make-k-rand val-of-rator^ k^)
                 (begin
                   [set! *k-clos* k^]
                   [set! *closure-cps* val-of-rator^]
                   [set! *val-clos* *val-apply-k*]
                   (apply-closure))]
                [(make-k-rator rand^ env^ k^)
                 (begin
                   [set! *k* (kt_make-k-rand *val-apply-k* k^)]
                   [set! *expr^* rand^]
                   [set! *env* env^]
                   (value-of-cps))]
                [(empty-k) *val-apply-k*])))

(define main
  (lambda ()
    (begin
      [set! *expr^* (expr_let
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
                      (expr_const 5)))]
      [set! *env* (envr_empty-env)]
      [set! *k* (kt_empty-k)]
      (value-of-cps))))

(main)