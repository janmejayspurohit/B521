#lang racket
(require racket/trace)

; Q1. list-ref
(define list-ref
  (λ (ls n)
    (letrec
        ([nth-cdr (λ (n)
                    (cond
                      [(zero? n) ls]
                      [else (cdr (nth-cdr (sub1 n)))])
                    )])
      (car (nth-cdr n)))))

(printf "----------Q1----------\n")
(list-ref '(a b c) 1)


; Q2. union
(define union
  (λ (lst1 lst2)
    (cond
      [(null? lst1) lst2]
      [(eqv? (memv (car lst1) lst2) #f)
       (cons (car lst1) (union (cdr lst1) lst2))]
      [else (union (cdr lst1) lst2)])))

(printf "----------Q2----------\n")
(union '(x y) '(x z))
      

; Q3. stretch
(define stretch
  (λ (fn val)
    (λ (n)
      (cond
        [(fn n)]
        [(eqv? n val)]
        [else #f]))))

(printf "----------Q3----------\n")
((stretch even? 1) 0)
((stretch even? 1) 3)
(filter (stretch (stretch even? 1) 3) '(0 1 2 3 4 5))


; Q4. walk-symbol
(define walk-symbol
  (λ (x lst)
    (cond
      [(eqv? (assv x lst) #f) x]
      [(pair? x) (cdr x)]
      [else (walk-symbol (cdr (assv x lst)) lst)])))

(printf "----------Q4----------\n")
(walk-symbol 'a '((a . 5) (b . 6) (c . a)))
(walk-symbol 'b '((a . 5) (b . ((c . a))) (c . a)))


; Q5. lambda-exp?
(define lambda-exp?
  (λ (E)
    (letrec
        ([p
          (λ (e)
            (match e
              [`,y #:when (symbol? y) #t]
              [`(lambda (,x) ,body) (and (p body)(symbol? x))]
              [`(,rator ,rand) (and (p rator) (p rand))]
              [else #f]))])
      (p E))))

(printf "----------Q5----------\n")
(lambda-exp? '(lambda (z) ((lambda (y) (a z)) (h (lambda (x) (h a))))))
(lambda-exp? '((lambda (5) x) (lambda (x) x)))


; Q6. Var Occurence
(define var-occurs?
  (λ (var fn)
    (match fn
      [`,y #:when(symbol? y) (eqv? var y)]
      [`(lambda (,x) ,body) #:when (symbol? x) (var-occurs? var body)]
      [`(,rator ,rand)
       (or (var-occurs? var rator) (var-occurs? var rand))])))

(printf "----------Q6----------\n")
(var-occurs? 'x '(lambda (x) y))
(var-occurs? 'x '((z y) x))


; Q7. vars
(define vars
  (λ (e)
    (match e
      [`,y #:when (symbol? y) (cons y '())]
      [`(lambda (,x) ,body) #:when (symbol? x) (append '() (vars body))]
      [`(,rator ,rand)
       (append (vars rator) (vars rand))]
      )))
(printf "----------Q7----------\n")
(vars '((lambda (y) (x x)) (x y)))


; Q8. unique-vars
(define unique-vars
  (λ (e)
    (match e
      [`,y #:when (symbol? y) (cons y '())]
      [`(lambda (,x) ,body) #:when (symbol? x) (append '() (unique-vars body))]
      [`(,rator ,rand)
       (union (unique-vars rator) (unique-vars rand))]
      [else '()]
      )))
(printf "----------Q8----------\n")
(unique-vars '((lambda (z) (lambda (y) (z y))) x))

      
; Q9. Free var
(define var-occurs-free?
  (lambda (var fn)
    (match fn
      [`,y #:when(symbol? y)
           (eqv? y var)]
      [`(lambda (,x) ,body) #:when (symbol? x)
                            (and (not (eqv? x var)) (var-occurs-free? var body))]
      [`(,rator ,rand)
       (or (var-occurs-free? var rator) (var-occurs-free? var rand))])))

(printf "----------Q9----------\n")
(var-occurs-free? 'x '(lambda (x) (lambda (x) x)))
(var-occurs-free? 'y '((lambda (y) (x y)) (lambda (x) (x y))))


; Q10. Bound var
(define var-occurs-bound?
  (lambda (var fn)
    (match fn
      [`,y #:when(symbol? y) #f]
      [`(lambda (,x) ,body) #:when (symbol? x)
                            (or (and (eqv? x var) (var-occurs-free? var body))
                                (var-occurs-bound? var body))]
      [`(,rator ,rand)
       (or (var-occurs-bound? var rator) (var-occurs-bound? var rand))])))

(printf "----------Q10----------\n")
(var-occurs-bound? 'x '((lambda (x) (x x)) (x x)))
(var-occurs-bound? 'x '(lambda (x) y))


; Q11. unique-free-vars
; From a1.rkt
(define remv-1st
  (λ (x lst)
    (cond
      [(null? lst) '()]
      [(eqv? (car lst) x) (cdr lst)]
      [else (cons (car lst)(remv-1st x (cdr lst)))])))

(define unique-free-vars
  (λ (exp)
    (match exp
      [`,y #:when (symbol? y) (cons y '())]
      [`(lambda (,x) ,body) #:when (symbol? x)
                            (remv-1st x (unique-free-vars body))]
      [`(,rator ,rand)
       (union (unique-free-vars rator) (unique-free-vars rand))])))

(printf "----------Q11----------\n")
(unique-free-vars '(lambda (x) (x y)))
(unique-free-vars '((lambda (x) ((x y) e)) (lambda (c) (x (lambda (x) (x (e x)))))))


; Q12. unique-bound-vars
(define unique-bound-vars
  (λ (exp)
    (match exp
      [`(lambda (,x) ,body) (cond
                              ((memv x (unique-vars body))             
                               (cons x (unique-bound-vars body)))       
                              (else
                               (unique-bound-vars body)))]
      [`(,rator ,rand)
       (union (unique-bound-vars rator) (unique-bound-vars rand))]
      [else `()])))


(printf "----------Q12----------\n")
(unique-bound-vars '((lambda (x) ((x y) e)) (lambda (c) (x (lambda (x) (x (e c)))))))
(unique-bound-vars '(lambda (y) y))
(unique-bound-vars '(lambda (x) (y z)))


; Q13. t-fact
(define t-fact
  (λ (n result)
    (cond
      [(eqv? n 0) result]
      [else (t-fact (sub1 n) (* result n))])))

(printf "----------Q13----------\n")
(t-fact 5 1)

