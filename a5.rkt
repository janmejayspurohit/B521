#lang racket
(require racket/trace)

(define val-of-cbv
  (λ (e env)
    (match e
      [`,n #:when (number? n) n]
      [`,b #:when (boolean? b) b]
      [`,y #:when (symbol? y) (unbox (apply-env env y))]
      [`(if ,condt ,texp ,fexp) (apply-if condt texp fexp env)]
      [`(zero? ,x) (apply-zero x env)]
      [`(* ,x ,y) (* (val-of-cbv x env) (val-of-cbv y env))]
      [`(sub1 ,n) #:when (or (number? n) (symbol? n)) (sub1 (val-of-cbv n env))]
      [`(let ([,x ,val]) ,body) (apply-let val env body x) ]
      [`(random ,x) (random (val-of-cbv x env))]
      [`(begin2 ,exp1 ,exp2)
       (begin (val-of-cbv exp1 env) (val-of-cbv exp2 env))]
      [`(set! ,x ,v) #:when (symbol? x)
                     (set-box! (apply-env env x) (val-of-cbv v env))]
      [`(lambda (,x) ,body) #:when (symbol? x) (make-clos body x env)]
      [`(,rator ,rand) (apply-clos (val-of-cbv rator env) (box (val-of-cbv rand env)))])))

(define make-clos
  (lambda (body x env)
    `(make-clos ,body ,x ,env)))

(define apply-clos
  (lambda (rator rand)
    (match rator
      [`(make-clos ,body ,x ,env)
       (val-of-cbv body (extend-env x rand env))])))

(define apply-let
  (lambda (val env body x)
    (let ([v (val-of-cbv val env)])
      (val-of-cbv body
                  (λ (arg)
                    (if (eqv? arg x) v
                        (env arg)))))))

(define apply-zero
  (lambda (x env)
    (cond
      [(eqv? (val-of-cbv x env) 0) #t]
      [else #f])))

(define apply-if
  (lambda (condt texp fexp env)
    (cond
      [(eqv? (val-of-cbv condt env) #t)
       (val-of-cbv texp env)]
      [else (val-of-cbv fexp env)])))

(define apply-env
  (lambda (env y)
    (match env
      [`(extend-env ,x ,arg ,env^)
       (cond
         [(eqv? y x) arg]
         [else (apply-env env^ y)])]
      [else (env y)])
    ))

(define extend-env
  (λ (x arg env)
    `(extend-env ,x ,arg ,env)))


(define empty-env
  (lambda ()
    `(empty-env)))


(val-of-cbv
 '((lambda (f)
     ((lambda (g)
        ((lambda (z) (begin2
                       (g z)
                       z))
         55))
      (lambda (y) (f y)))) (lambda (x) (set! x 44)))
 (empty-env))



(define apply-clos-cbr
  (lambda (rator rand)
    (match rator
      [`(make-clos ,body ,x ,env)
       (val-of-cbr body (extend-env x rand env))])))

(define val-of-cbr
  (λ (e env)
    (match e
      [`,n #:when (number? n) n]
      [`,b #:when (boolean? b) b]
      [`,y #:when (symbol? y) (unbox (apply-env env y))]
      [`(if ,condt ,texp ,fexp) (apply-if condt texp fexp env)]
      [`(zero? ,x) (apply-zero x env)]
      [`(* ,x ,y) (* (val-of-cbr x env) (val-of-cbr y env))]
      [`(sub1 ,n) #:when (or (number? n) (symbol? n)) (sub1 (val-of-cbr n env))]
      [`(let ([,x ,val]) ,body) (apply-let val env body x) ]
      [`(random ,x) (random (val-of-cbr x env))]
      [`(begin2 ,exp1 ,exp2)
       (begin (val-of-cbr exp1 env) (val-of-cbr exp2 env))]
      [`(set! ,x ,v) #:when (symbol? x)
                     (set-box! (apply-env env x) (val-of-cbr v env))]
      [`(lambda (,x) ,body) #:when (symbol? x) (make-clos body x env)]
      [`(,rator ,rand) #:when (symbol? rand)
                       (apply-clos-cbr (val-of-cbr rator env)(apply-env env rand))]
      [`(,rator ,rand) (apply-clos-cbr (val-of-cbr rator env) (box (val-of-cbr rand env)))]
      )))


(val-of-cbr
 '((lambda (a)
     ((lambda (p)
        (begin2
          (p a)
          a)) (lambda (x) (set! x 4)))) 3)
 (empty-env))



(define val-of-cbname
  (λ (e env)
    (match e
      [`,n #:when (number? n) n]
      [`,b #:when (boolean? b) b]
      [`,y #:when (symbol? y) ((unbox (apply-env env y)))]
      [`(if ,condt ,texp ,fexp) (apply-if-cbname condt texp fexp env)]
      [`(zero? ,x) (apply-zero x env)]
      [`(* ,x ,y) (* (val-of-cbname x env) (val-of-cbname y env))]
      [`(sub1 ,n) #:when (or (number? n) (symbol? n)) (sub1 (val-of-cbname n env))]
      [`(let ([,x ,val]) ,body) (apply-let val env body x) ]
      [`(random ,x) (random (val-of-cbname x env))]
      [`(begin2 ,exp1 ,exp2)
       (begin (val-of-cbname exp1 env) (val-of-cbname exp2 env))]
      [`(set! ,x ,v) #:when (symbol? x)
                     (set-box! (apply-env env x) (val-of-cbname v env))]
      [`(lambda (,x) ,body) #:when (symbol? x) (make-clos body x env)]
      [`(,rator ,rand) (apply-clos-cbname (val-of-cbname rator env) (box (lambda() (val-of-cbname rand env))))])))

(define apply-if-cbname
  (lambda (condt texp fexp env)
    (cond
      [(eqv? (val-of-cbname condt env) #t)
       (val-of-cbname texp env)]
      [else (val-of-cbname fexp env)])))

(define apply-clos-cbname
  (lambda (rator rand)
    (match rator
      [`(make-clos ,body ,x ,env)
       (val-of-cbname body (extend-env x rand env))])))

(define random-sieve
  '((lambda (n)
      (if (zero? n)
          (if (zero? n) (if (zero? n) (if (zero? n) (if (zero? n) (if (zero? n) (if (zero? n) #t #f) #f) #f) #f) #f) #f)
          (if (zero? n) #f (if (zero? n) #f (if (zero? n) #f (if (zero? n) #f (if (zero? n) #f (if (zero? n) #f #t))))))))
    (random 2)))

(val-of-cbname random-sieve (empty-env))



(define val-of-cbneed
  (λ (e env)
    (match e
      [`,n #:when (number? n) n]
      [`,b #:when (boolean? b) b]
      [`,y #:when (symbol? y) (((lambda (b) (let ([val ((unbox b))])(set-box! b (lambda () val))val))) (apply-env env y))]
      [`(if ,condt ,texp ,fexp) (apply-if-cbname condt texp fexp env)]
      [`(zero? ,x) (apply-zero x env)]
      [`(* ,x ,y) (* (val-of-cbneed x env) (val-of-cbneed y env))]
      [`(sub1 ,n) #:when (or (number? n) (symbol? n)) (sub1 (val-of-cbneed n env))]
      [`(let ([,x ,val]) ,body) (apply-let val env body x) ]
      [`(random ,x) (random (val-of-cbneed x env))]
      [`(begin2 ,exp1 ,exp2)
       (begin (val-of-cbneed exp1 env) (val-of-cbneed exp2 env))]
      [`(set! ,x ,v) #:when (symbol? x)
                     (set-box! (apply-env env x) (val-of-cbneed v env))]
      [`(lambda (,x) ,body) #:when (symbol? x) (make-clos body x env)]
      [`(,rator ,rand) (apply-clos-cbname (val-of-cbneed rator env) (box (lambda() (val-of-cbneed rand env))))])))


(val-of-cbname
 '((lambda (z) 100)
   ((lambda (x) (x x)) (lambda (x) (x x))))
 (empty-env))



(define value-of-ds
  (λ (e env)
    (match e
      [`,n #:when (number? n) n]
      [`,b #:when (boolean? b) b]
      [`,y #:when (symbol? y) (apply-env-ds env y)]
      [`(+ ,nexp1 ,nexp2)
       (+ (value-of-ds nexp1 env) (value-of-ds nexp2 env))]
      [`(if ,condt ,texp ,fexp) (apply-if condt texp fexp env)]
      [`(zero? ,x) (apply-zero x env)]
      [`(* ,x ,y) (* (value-of-ds x env) (value-of-ds y env))]
      [`(letrec ,1/2-closures ,b)
       (value-of-ds b (ext-rec-env 1/2-closures env))]
      [`(sub1 ,n) #:when (or (number? n) (symbol? n)) (sub1 (value-of-ds n env))]
      [`(let ([,x ,val]) ,body) (apply-let val env body x) ]
      [`(lambda (,x) ,body) #:when (symbol? x) (make-clos body x env)]
      [`(,rator ,rand) (apply-clos (value-of-ds rator env) (value-of-ds rand env))])))


(define ext-rec-env
  (λ (1/2-closures env)
    `(rec-env ,1/2-closures ,env)))

(define apply-env-ds
  (λ (env y)
    (match env
      [`(rec-env ,1/2-closures ,env^)
       (cond
         [(assv y 1/2-closures)
          => (λ (p)
               (let ([1/2-closure (cadr p)]) (value-of-ds 1/2-closure env)))]
         [else (apply-env-ds env^ y)])]
      )))

(define extend-env-ds
  (lambda (x arg env)
    (lambda (y)
      (cond
        [(eqv? y x) arg]
        [else (apply-env-ds env y)]))))

(define empty-env-ds
  (lambda ()
    (lambda (y) `(error "variable not found ~s" y))))

(value-of-ds '(letrec ([x 22]
                       [y 20]
                       [z (+ x y)])
                z)
             (empty-env-ds))

(define val-of-cbv-cons
  (λ (e env)
    (match e
      [`,n #:when (number? n) n]
      [`,b #:when (boolean? b) b]
      [`,y #:when (symbol? y) (unbox (apply-env env y))]
      [`(if ,condt ,texp ,fexp) (apply-if condt texp fexp env)]
      [`(zero? ,x) (apply-zero x env)]
      [`(* ,x ,y) (* (val-of-cbv-cons x env) (val-of-cbv-cons y env))]
      [`(sub1 ,n) #:when (or (number? n) (symbol? n)) (sub1 (val-of-cbv-cons n env))]
      [`(let (,argSymbol) ,body) ( λ (valueOfArg)
                                    (val-of-cbv-cons body
                                                     (λ (varToQuery)                   
                                                       (if (eqv? varToQuery argSymbol) 
                                                           valueOfArg
                                                           (env varToQuery)            
                                                           ))))]
      [`(random ,x) (random (val-of-cbv-cons x env))]
      [`(begin2 ,exp1 ,exp2)
       (begin (val-of-cbv-cons exp1 env) (val-of-cbv-cons exp2 env))]
      [`(cons ,head ,rest) (cons (val-of-cbv-cons head env) (val-of-cbv-cons rest env))]
      [`(cons^ ,head ,rest) (cons (box (λ()(val-of-cbv-cons head env))) (box (λ()(val-of-cbv-cons rest env))))]
      [`(car ,lst) (car lst)]
      [`(cdr ,lst) (cdr lst)]
      [`(car^ ,lst) ((unbox (car (val-of-cbv-cons lst env))))]
      [`(cdr^ ,lst) ((unbox (cdr (val-of-cbv-cons lst env))))]          
      [`(null? ,lst) (null? (val-of-cbv-cons lst env))]
      [`(add1 ,exp) (add1 (val-of-cbv-cons exp env))]
      [`(quote ,v) v]
      [`(set! ,x ,v) #:when (symbol? x)
                     (set-box! (apply-env env x) (val-of-cbv-cons v env))]
      [`(lambda (,argSymbol) ,body) ( λ (arg)
                                       (val-of-cbv-cons body
                                                        (λ (v)                   
                                                          (if (eqv? v argSymbol)
                                                              arg
                                                              (env v)
                                                              ))))]
      [`(,rator ,rand) (val-of-cbv-cons rator env) (box (val-of-cbv-cons rand env))])))

(define apply-let-cbv-cons
  (lambda (val env body x)
    (let ([v (val-of-cbv-cons val env)])
      (val-of-cbv-cons body
                       (λ (arg)
                         (if (eqv? arg x) v
                             (env arg)))))))

(define cons-test
  (quote
   (let ((fix (lambda (f)
                ((lambda (x) (f (lambda (v) ((x x) v))))
                 (lambda (x) (f (lambda (v) ((x x) v))))))))
     (let ((map (fix (lambda (map)
                       (lambda (f)
                         (lambda (l)
                           (if (null? l)
                               '()
                               (cons^ (f (car^ l))
                                      ((map f) (cdr^ l))))))))))
       (let ((take (fix (lambda (take)
                          (lambda (l)
                            (lambda (n)
                              (if (zero? n)
                                  '()
                                  (cons (car^ l) 
                                        ((take (cdr^ l)) (sub1 n))))))))))
         ((take ((fix (lambda (m)
                        (lambda (i)
                          (cons^ 1 ((map (lambda (x) (add1 x))) (m i)))))) 0)) 5))))))


(val-of-cbv-cons cons-test (empty-env))