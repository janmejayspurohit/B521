#lang pie

; Q1.
(claim intriguing-word Atom)
(define intriguing-word 'molecule)
intriguing-word

; Q2.
(claim lucky-num Nat)
(define lucky-num 2)
lucky-num

; Q3.
(claim to-go-order (Pair Nat Atom))
(define to-go-order (cons 8 'molecule))
to-go-order

; Q4.
(claim MyFirstType U)
(define MyFirstType (Pair Nat (Pair Nat Atom)))
MyFirstType

; Q5.
(claim my-thing-and-Atom (Pair MyFirstType U))
(define my-thing-and-Atom (cons (cons 10 (cons 29 'hbd)) Atom))
my-thing-and-Atom

; Q6.
(claim with-Nats
  (-> (-> Nat Nat
        Nat)
      (Pair Nat Nat)
    Nat))

(define with-Nats
  (lambda (binfn nats)
    (binfn (car nats) (cdr nats))))

(check-same Nat (with-Nats (λ (n m) n) (cons 1 2)) 1)
(check-same Nat (with-Nats (λ (n m) (add1 m)) (cons 1 2)) 3)

; Q7.
(claim at-least-two?
  (-> Nat
    Atom))

(define at-least-two?
  (lambda (n)
    (which-Nat n 'nil
      (lambda (smaller)
        (which-Nat smaller 'nil
          (lambda (s) 't))))))

(check-same Atom (at-least-two? 0) 'nil)
(check-same Atom (at-least-two? 1) 'nil)
(check-same Atom (at-least-two? 41) 't)

; Q8.
(claim + (-> Nat Nat
           Nat))
(define + (λ (n m) (rec-Nat n
                     m
                     (λ (k k+m) (add1 k+m)))))
 
(claim * (-> Nat Nat
           Nat))
(define * (λ (n m) (rec-Nat n
                     0
                     (λ (k k*m) (+ m k*m)))))

(claim expt (-> Nat Nat Nat))
(define expt
  (lambda (m n)
    (rec-Nat n 1 (lambda (k m-to-n-1)
                   (* m m-to-n-1)))))

(expt 9 2) 
(expt 9 0)
(expt 0 0)
(expt 0 2)

; Q9.
(claim map
  (Π ((A U)
      (B U))
    (→ (→ A B) (List A)
      (List B))))

(define map
  (lambda (A B)
    (lambda(f list-A)
      (rec-List list-A
        (the (List B) nil)
        (lambda (a d cdrB)
          (:: (f a) cdrB))))))

(check-same (List Nat) (map Nat Nat (lambda (a) (add1 a)) nil) (the (List Nat) nil))
(check-same (List Nat) (map Nat Nat (lambda (a) (add1 a)) (:: 1 nil)) (:: 2 nil))
(check-same (List Nat) (map Nat Nat (lambda (a) (* a 7)) (:: 7 (:: 14 nil))) (:: 49 (:: 98 nil)))

; Q10.
(claim nth
  (Π ((A U))
    (→ (List A) A Nat
      A)))

(claim nth-cdr
  (Π ((A U))
    (-> (List A) Nat
      (List A))))

(define nth-cdr
  (lambda (A ls n)
    (rec-Nat n
      ls
      (lambda (n-1 n-1th-cdr)
        (rec-List (the (List A) n-1th-cdr)
          (the (List A) nil)
          (lambda (a d _)
            d))))))

(define nth
  (lambda (A ls default n)
    (rec-List (nth-cdr A ls n)
      default
      (lambda (a d _)
        a))))

(check-same Nat (nth Nat nil 3 0) 3)
(check-same Nat (nth Nat nil 2 9) 2)
(check-same Nat (nth Nat (:: 1 nil) 1234 1) 1234)

; Q11.
(claim vec-second
  (Π ((A U)
      (n Nat))
    (-> (Vec A (add1 (add1 n)))
      A)))

(define vec-second
  (lambda (A n)
    (lambda (vec2)
      (head (tail vec2)))))

(check-same Nat (vec-second Nat 0 (vec:: 1 (vec:: 8 vecnil))) 8)
(check-same Nat (vec-second Nat 2 (vec:: 3 (vec:: 7 (vec:: 1 (vec:: 8 vecnil))))) 7)
(check-same Atom (vec-second Atom 2 (vec:: 'a (vec:: 's (vec:: 'd (vec:: 'f vecnil))))) 's)