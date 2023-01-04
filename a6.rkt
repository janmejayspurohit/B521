#lang racket
(require racket/trace)

; Q1.
(define empty-k
  (lambda ()
    (let ((once-only #f))
      (lambda (v)
        (if
         once-only
         (error 'empty-k "You can only invoke the empty continuation once")
         (begin (set! once-only #t) v))))))

(define binary-to-decimal-cps
  (lambda (n k)
    (cond
      [(null? n) (k 0)]
      [else (binary-to-decimal-cps (cdr n) (λ (v)
                                             (k (+ (car n) (* 2 v)))))])))

(printf "----------Q1-----------\n")
(binary-to-decimal-cps '() (empty-k))
(binary-to-decimal-cps '(1) (empty-k))
(binary-to-decimal-cps '(0 1) (empty-k))
(binary-to-decimal-cps '(1 1 0 1) (empty-k))

; Q2.
(define star-cps
  (lambda (m k)
    (k (lambda (n k^)
         k^ (* m n)))))

(printf "----------Q2-----------\n")
((star-cps 2 (empty-k)) 3 (empty-k))
((star-cps ((star-cps 2 (empty-k)) 3 (empty-k)) (empty-k)) 5 (empty-k))

; Q3.
(define times-cps
  (lambda (ls k)
    (cond
      [(null? ls) (k 1)]
      [(zero? (car ls)) (k 0)]
      [else (times-cps (cdr ls) (λ (v)
                                  (k (* (car ls) v))))])))

(printf "----------Q3-----------\n")
(times-cps '(1 2 3 4 5) (empty-k))
(times-cps '(1 2 3 0 3) (empty-k))

; Q4.
(define times-cps-2
  (lambda (ls k)
    (cond
      [(null? ls) (k 1)]
      [(zero? (car ls)) 0]
      [else (times-cps (cdr ls) (λ (v)
                                  (k (* (car ls) v))))])))

(printf "----------Q4-----------\n")
(times-cps-2 '(1 2 3 4 5) (empty-k))
(times-cps-2 '(1 2 3 0 3) (empty-k))

; Q5.
(define remv-first-9*-cps
  (lambda (ls k)
    (cond
      [(null? ls) (k '())]
      [(pair? (car ls))
       (cond
         [
          (remv-first-9*-cps (car ls) (λ (v)
                                        (equal? (car ls) v)))
          (remv-first-9*-cps (cdr ls) (λ (v)
                                        (k (cons (car ls) v))) )]
         [else  (remv-first-9*-cps (car ls) (λ (v)
                                              (k (cons v (cdr ls)))))])]
      [(eqv? (car ls) '9) (k (cdr ls))]
      [else
       (remv-first-9*-cps (cdr ls) (λ (v)
                                     (k (cons (car ls) v))))])))

(printf "----------Q5-----------\n")
(remv-first-9*-cps '((1 2 (3) 9)) (empty-k))
(remv-first-9*-cps '(9 (9 (9 (9)))) (empty-k))
(remv-first-9*-cps '(((((9) 9) 9) 9) 9) (empty-k))

; Q6.
(define cons-cell-count-cps
  (lambda (ls k)
    (cond
      [(pair? ls)
       (cons-cell-count-cps (car ls) (lambda (v)
                                       (cons-cell-count-cps (cdr ls) (lambda (w)
                                                                       (k (add1(+ w v)))))))]
      [else (k 0)])))

(printf "----------Q6-----------\n")
(cons-cell-count-cps '(1 2 3 4) (empty-k))
(cons-cell-count-cps '(1 2 (3 (4) 5) 4 ()) (empty-k))

; Q7.
(define find-cps
  (lambda (u s k)
    (let ((pr (assv u s)))
      (if pr (find-cps (cdr pr) s k) (k u)))))

(printf "----------Q7-----------\n")
(find-cps 5 '((5 . a) (6 . b) (7 . c)) (empty-k))
(find-cps 5 '((5 . 6) (9 . 6) (2 . 9)) (empty-k))

; Q8.
(define ack-cps
  (lambda (m n k)
    (cond
      [(zero? m) (k (add1 n))]
      [(zero? n) (ack-cps (sub1 m) 1 k)]
      [else (ack-cps m (sub1 n) (lambda (v) (ack-cps (sub1 m) v (lambda (w) (k w)))))])))

(printf "----------Q8-----------\n")
(ack-cps 2 3 (empty-k))

; Q9.
(define fib-cps
  (λ (n k)
    ((lambda (fib-cps)
       (fib-cps fib-cps n k))
     (lambda (fib-cps n k )
       (cond
         [(zero? n) (k 0)]
         [(zero? (sub1 n)) (k 1)]
         [else (fib-cps fib-cps (sub1 n)
                        (λ (v)
                          (fib-cps fib-cps (sub1 (sub1 n))
                                   (λ (w)
                                     (k (+ v w))))))])))))

(printf "----------Q9-----------\n")
(fib-cps 12 (empty-k))

; Q10.
(define unfold-cps
  (lambda (p f g seed k)
    ((lambda (h)
       ((h h) seed '() k))
     (lambda (h)
       (lambda (seed ans k)
         (p seed
            (λ (v)
              (if v
                  (k ans)
                  (g seed (λ (w)
                            (f seed (λ (u)
                                      ((h h) w (cons u ans) k)))))))))))))

(define null?-cps
  (lambda (ls k)
    (k (null? ls))))
(define car-cps
  (lambda (pr k)
    (k (car pr))))
(define cdr-cps
  (lambda (pr k)
    (k (cdr pr))))

(printf "----------Q10-----------\n")
(unfold-cps null?-cps car-cps cdr-cps '(a b c d e) (empty-k))

; Q11.
(define empty-s
  (lambda ()
    '()))

(define unify-cps
  (lambda (u v s k)
    (cond
      ((eqv? u v) s)
      ((number? u) (k (cons (cons u v) s)))
      ((number? v) (unify-cps v u s
                              (λ (v)
                                (k v))))
      ((pair? u)
       (if (pair? v)
           (let ([s
                  (find-cps (car u) s
                            (λ (find1)
                              (find-cps (car v) s
                                        (λ (find2)
                                          (unify-cps find1 find2 s
                                                     (λ (w)
                                                       (k w)))))))])
             (if s
                 (find-cps (cdr u) s
                           (λ (find1)
                             (find-cps (cdr v) s
                                       (λ (find2)
                                         (unify-cps find1 find2 s
                                                    (λ (w)
                                                      (k w)))))))
                 #f))
           #f))
      (else #f))))

(printf "----------Q11-----------\n")
(unify-cps '(1 2 3) '(x 1 2) (empty-s) (λ (x) x))
(unify-cps 'x 'y (empty-s) (empty-k))

; Q12.
(define M
  (lambda (f)
    (lambda (ls)
      (cond
        ((null? ls) '())
        (else (cons (f (car ls)) ((M f) (cdr ls))))))))

(define M-cps
  (lambda (f k)
    (lambda (ls k^)
      (cond
        ((null? ls) (k^ `()))
        (else ((M-cps f k) (cdr ls) (λ (v)
                                      (k^ (cons (f (car ls)) v)))))))))

(printf "----------Q12-----------\n")
((M (λ (x) (+ x 2))) '(1 2 3))
((M-cps (λ (x) (+ x 2)) (empty-k)) '(1 2 3) (empty-k))

; Q13.
(define use-of-M
  ((M (lambda (n) (add1 n))) '(1 2 3 4 5)))

(printf "----------Q13-----------\n")
(print use-of-M)
(printf "\n")

(define use-of-M-cps
  ((M-cps (λ (x) (+ x 2)) (empty-k)) '(1 2 3) (empty-k)))

(print use-of-M-cps)
(printf "\n")

; Q14.
(define strange
  (lambda (x)
    ((lambda (g) (lambda (x) (g g)))
     (lambda (g) (lambda (x) (g g))))))

(printf "----------Q14-----------\n")
((((((strange 5) 6) 7) 8) 9) 10)

(define strange-cps
  (λ (x k1)
    ((λ (g k2)
       (k2 (λ (x k3) (g g k3))))
     (λ (g k4)
       (k4 (λ (x k5) (g g k5))))
     k1)))

((((((strange-cps 5 (empty-k)) 6 (empty-k)) 7 (empty-k)) 8 (empty-k)) 9 (empty-k)) 10 (empty-k))

; Q15.
(define use-of-strange
  (let ([strange^ (((strange 5) 6) 7)])
    (((strange^ 8) 9) 10)))

(define use-of-strange-cps
  (let ([strange^ (((strange-cps 5 (empty-k)) 6 (empty-k)) 7 (empty-k))])
    (((strange^ 8 (empty-k)) 9 (empty-k)) 10 (empty-k))))
  

(printf "----------Q15-----------\n")
(print use-of-strange)
(print use-of-strange-cps)

; Q16.
(define why
  (lambda (f)
    ((lambda (g)
       (f (lambda (x) ((g g) x))))
     (lambda (g)
       (f (lambda (x) ((g g) x)))))))

(define why-cps
  (lambda (f k)
    ((lambda (g k)
       (f (lambda (x k) (g g) x) k))
     (lambda (g k)
       (f (lambda (x k) (g g) x) k)))))

(define almost-length
  (lambda (f)
    (lambda (ls)
      (if (null? ls)
          0
          (add1 (f (cdr ls)))))))

(printf "\n----------Q16-----------\n")
((why almost-length) '(a b c d e))

; Q17.
(define why-cps-cps
  (lambda (f k k^)
    ((lambda (g k k^)
       (f (lambda (x k k^) (g g (lambda (v) (v x k k^)))) k k^))
     (lambda (g k k^)
       (f (lambda (x k k^) (g g (lambda (v) (v x k k^)))) k k^)) k k^)))