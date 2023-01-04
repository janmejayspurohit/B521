#lang racket
(require "mk.rkt")
(require "monads.rkt")

; Q1.
(defrel (listo ls)
  (conde
   [(== '() ls)]
   [(=/= '() ls)
    (fresh (a d res)
           (== `(,a . ,d) ls)
           (listo d))]))

(printf "\n------------------Q1------------------\n")
(run! 1 q (listo '(a b c d e)))
(run! 1 q (listo '(a b c d . e)))
(run! 4 q (listo q))
(run! 4 q (listo `(a b ,q)))


; Q3.
(define (findf-maybe p ls)
  (cond
    [(null? ls) (Nothing)]
    [(p (car ls)) (Just (car ls))]
    [else (findf-maybe p (cdr ls))]))

(printf "\n------------------Q3------------------\n")
(findf-maybe symbol? '(1 2 c))
(findf-maybe boolean? '(#f 1 2 c)) 
(findf-maybe number? '(a b c))


; Q4.
(define (partition-writer condition ls)
  (cond
    [(null? ls) (inj-writer '())]
    [(condition
      (car ls))
     (go-on
      (d <- (partition-writer condition (cdr ls)))
      (inj-writer  `(,(car ls) . ,d)))]
    [else
     (go-on
      (tell (car ls))
      (d <-(partition-writer condition (cdr ls)))
      (inj-writer d))]))

(printf "\n------------------Q4------------------\n")
(run-writer (partition-writer even? '(1 2 3 4 5 6 7 8 9 10)))
(run-writer (partition-writer odd? '(1 2 3 4 5 6 7 8 9 10)))


; Q5.
(define powerXpartials
  (lambda (x n)
    (cond
      [(zero? n) (inj-writer 1)]
      [(zero? (sub1 n)) (inj-writer x)]
      [(odd? n)
       (go-on
        (s <- (powerXpartials x (sub1 n)))
        (tell s)
        (inj-writer (* x s)))]
      [(even? n)
       (go-on
        (y <- (powerXpartials x (/ n 2)))
        (tell y)
        (inj-writer (* y y)))])))

(printf "\n------------------Q5------------------\n")
(run-writer (powerXpartials 2 6))
(run-writer (powerXpartials 3 5))
(run-writer (powerXpartials 5 7))


; Q6.
(define (replace-with-count s ls)
  (match ls
    [`,y
     (go-on
      (val <- (get))
      (put (if (eqv? s y)
               (add1 val)
               val))
      (inj-state (if (eqv? s y) val  y)))]
    [`(,a . ,d)
     (go-on
      (aa <- (replace-with-count s a))
      (dd <- (replace-with-count s d))
      (inj-state `(,aa . ,dd)))]))

(printf "\n------------------Q6------------------\n")
((run-state (replace-with-count 'o '(a o (t o (e o t ((n . m) . o) . f) . t) . r))) 0)
((run-state (replace-with-count 'o '(((h (i s . o) . a) o s o e . n) . m))) 0)
((run-state (replace-with-count 'o '(o (h (o s . o) . o) . o))) 1)


; Q7.
(define (reciprocal n)
  (if (zero? n)
      (Nothing)
      (Just (/ 1 n))))

(printf "\n------------------Q7------------------\n")
(reciprocal 0)
(reciprocal 2)

(define traverse
  (lambda (op bind f)
    (letrec
        ((recurse
          (lambda (tree)
            (cond
              [(pair? tree)
               (go-on
                [a <- (recurse (car tree))]
                [d <- (recurse (cdr tree))]
                (op (cons a d)))]
              [else (f tree)]))))
      recurse)))

(define traverse-reciprocal
  (traverse Just bind-maybe reciprocal))

(traverse-reciprocal '((1 . 2) . (3 . (4 . 5))))
(traverse-reciprocal '((1 . 2) . (0 . (4 . 5))))


; Q8.
(define (halve n)
  (cond
    [(even? n) (inj-writer (/ n 2))]
    [else (go-on
           (tell n)
           (inj-writer n))]))

(printf "\n------------------Q8------------------\n")
(run-writer (halve 6))
(run-writer (halve 5))

(define traverse-halve
  (traverse inj-writer bind-writer halve))
 
(run-writer (traverse-halve '((1 . 2) . (3 . (4 . 5)))))


; Q9.
(define (state/sum n)
  (go-on (val <- (get))
         (put (+ val n))
         (inj-state val)))

(define traverse-state/sum
  (traverse inj-state bind-state state/sum))

((run-state (traverse-state/sum '((1 . 2) . (3 . (4 . 5))))) 0)

(printf "\n------------------Q9------------------\n")
((run-state (state/sum 5)) 0)
((run-state (state/sum 2)) 0)
((run-state (state/sum 2)) 3)


; Brainteaser

(define apply-closure
  (lambda (closure val)
    (closure val)))

(define extend-env
  (lambda (x arg env)
    (lambda (val)
      (if (eq? x val)
          arg
          (env val)))))

(define make-closure
  (lambda (x body env)
    (lambda (arg)
      (value-of-cps body (extend-env x arg env)))))

(define apply-env
  (lambda (env var)
    (env var)))

; go on not implemented
(define value-of-cps
  (lambda (expr env)
    (match expr
      [(? number?) (inj-k expr)]
      [(? boolean?) (inj-k expr)]       
      [(? symbol?) (inj-k (apply-env env expr))]
      [`(* ,x1 ,x2) (bind-k (value-of-cps x1 env)
                            (lambda (x)
                              (bind-k
                               (value-of-cps x2 env)
                               (lambda (x^)
                                 (inj-k (* x x^))))))]
      [`(sub1 ,x) (bind-k
                   (value-of-cps x env)
                   (lambda (x)
                     (inj-k (sub1 x))))]
      [`(zero? ,x) (bind-k
                    (value-of-cps x env)
                    (lambda (x)
                      (inj-k (zero? x))))]
      [`(if ,test ,conseq ,alt) (bind-k
                                 (value-of-cps test env)
                                 (lambda (x)
                                   (if x
                                       (value-of-cps conseq env)
                                       (value-of-cps alt env))))]
      [`(capture ,k-id ,body) (callcc (lambda (k)
                                        (value-of-cps body (extend-env k-id k env))))]
      [`(return ,k-exp ,v-exp) (bind-k
                                (value-of-cps k-exp env)
                                (lambda (x)
                                  (bind-k
                                   (value-of-cps v-exp env)
                                   (lambda (x^)
                                     (x x^)))))]
      [`(lambda (,x) ,body) (inj-k (make-closure x body env))]
      [`(,rator ,rand) (bind-k (value-of-cps rator env)
                               (lambda (x)
                                 (bind-k
                                  (value-of-cps rand env)
                                  (lambda (x^)
                                    (apply-closure x x^)))))])))

(define fact-5
  '((lambda (f)
      ((f f) 5))
    (lambda (f)
      (lambda (n)
        (if (zero? n)
            1
            (* n ((f f) (sub1 n))))))))

(define empty-env
  (lambda ()
    (lambda (var) (error "value does not exist" var))))

(define capture-fun
  '(* 3 (capture q (* 2 (return q 4)))))

(printf "\n------------------BrainTeaser------------------\n")
((run-k (value-of-cps fact-5 (empty-env))) (lambda (v) v))
((run-k (value-of-cps capture-fun (empty-env))) (lambda (v) v))