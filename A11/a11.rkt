#lang racket
(require "mk.rkt")
(require racket/trace)

#|
Please consider submission upto 3am
I had mid term yesterday
|#

(defrel (∈ Γ x τ)
  (fresh (xa τa restΓ)
         (== `((,xa . ,τa) . ,restΓ) Γ)
         (conde
          [(== x xa)
           (== τa τ)]
          [(=/= x xa)
           (∈ restΓ x τ)])))


(defrel (!- Γ e τ) 
  (conde
   [(symbolo e) (∈ Γ e τ)]

   [(fresh (f body)
           (symbolo f)
           (== `(fix (lambda (,f) ,body)) e)
           (!- `((,f . ,τ) . ,Γ) body τ))]

   [(fresh (ls at dt)
           (== `(car ,ls) e)
           (== at τ)
           (!- Γ ls `(pairof ,at ,dt)))]
   
   [(fresh (e1 e2 t1 t2)
           (== `(cons ,e1 ,e2) e)
           (== `(pairof ,t1 ,t2) τ)
           (!- Γ e1 t1)
           (!- Γ e2 t2))]
   
   [(fresh (ls at dt)
           (== `(cdr ,ls) e)
           (== dt τ)
           (!- Γ ls `(pairof ,at ,dt)))]

   [(numbero e) (== 'Nat τ)]
   
   [(conde
     [(== #t e)]
     [(== #f e)])
    (== 'Bool τ)]

   [(fresh (e^)
           (== `(zero? ,e^) e)
           (== 'Bool τ)
           (!- Γ e^ 'Nat))]
   
   [(fresh (e^)
           (== `(sub1 ,e^) e)
           (== 'Nat τ)
           (!- Γ e^ 'Nat))]
   
   [(fresh (e^)
           (== `(not ,e^) e)
           (== 'Bool τ)
           (!- Γ e^ τ))]
   
   [(fresh (test conseq alt)
           (== `(if ,test ,conseq ,alt) e)
           (!- Γ test 'Bool)
           (!- Γ conseq τ)
           (!- Γ alt τ))]
   
   [(fresh (e1 e2) 
           (== `(and ,e1 ,e2) e)
           (== 'Bool τ)
           (!- Γ e1 'Bool)
           (!- Γ e2 'Bool))]
   
   [(fresh (e1 e2) 
           (== `(or ,e1 ,e2) e)
           (== 'Bool τ)
           (!- Γ e1 'Bool)
           (!- Γ e2 'Bool))]
   
   [(fresh (e1 e2)
           (== `(+ ,e1 ,e2) e)
           (== 'Nat τ)
           (!- Γ e1 'Nat)
           (!- Γ e2 'Nat))]

   [(fresh (e1 e2)
           (== `(* ,e1 ,e2) e)
           (== 'Nat τ)
           (!- Γ e1 'Nat)
           (!- Γ e2 'Nat))]
   
   [(fresh (x body τx τbody)
           (== `(lambda (,x) ,body) e)
           (symbolo x)
           (== `(,τx -> ,τbody) τ)
           (!- `((,x . ,τx) . ,Γ) body τbody))]
   
   [(fresh (rator rand τx)
           (== `(,rator ,rand) e)
           (!- Γ rand τx)
           (!- Γ rator `(,τx -> ,τ)))]))


(run! 1 q (!- '() #t q))

(run! 1 q (!- '() 17 q))

(run! 1 q (!- '() '(zero? 24) q))

(run! 1 q (!- '() '(zero? (sub1 24)) q))

(run! 1 q (!- '() '(not (zero? (sub1 24))) q))

(run! 1 q
      (!- '() '(zero? (sub1 (sub1 18))) q))

(run! 1 q
      (!- '()  '(lambda (n) (if (zero? n) n n)) q))

(run! 1 q
      (!- '() '(lambda (n)
                 (lambda (b)
                   (if (and (not b) (zero? n))
                       n n))) q))

(run! 1 q
      (!- '() '((lambda (n) (zero? n)) 5) q))

(run! 1 q
      (!- '() '(if (zero? 24) 3 4) q))

(run! 1 q
      (!- '() '(if (zero? 24) (zero? 3) (zero? 4)) q))

(run! 1 q
      (!- '() '(lambda (x) (sub1 x)) q))

(run! 1 q (!- '() (and (zero? 5) (not #t)) q))

(run! 1 q (!- '() (or #f (not #t)) q))

(run! 1 q
      (!- '() '(lambda (a) (lambda (x) (+ a x))) q))

(run! 1 q
      (!- '() '(lambda (f)
                 (lambda (x)
                   ((f x) x)))
          q))

(run! 1 q
      (fresh (t)
             (!- '() '(lambda (f) (f f)) t)))

(length (car (run 20 (q)
                  (fresh (lam a b)
                         (!- '() `((,lam (,a) ,b) 5) 'Nat)
                         (== `(,lam (,a) ,b) q)))))

(length (car (run 30 q (!- '() q 'Nat))))

(length (car (run 30 q (!- '() q '(Nat -> Nat)))))

(length (car (run 500 q (!- '() q '(Nat -> Nat)))))

(length (car (run 30 q (!- '() q '(Bool -> Nat)))))

(length (car (run 30 q (!- '() q '(Nat -> (Nat -> Nat))))))

(length (car (run 100 q
                  (fresh (e t)
                         (!- '() e t)
                         (== `(,e ,t) q)))))

(length (car (run 100 q
                  (fresh (g e t)
                         (!- g e t)
                         (== `(,g ,e ,t) q)))))

(length
 (car (run 100 q
           (fresh (g v)
                  (!- g `(var ,v) 'Nat)
                  (== `(,g ,v) q)))))

(run! 1 q
        (fresh (g)
               (!- g
                   '((fix (lambda (!)
                            (lambda (n)
                              (if (zero? n)
                                  1
                                  (* n (! (sub1 n)))))))
                     5)
                   q)))

(run! 1 q
      (fresh (g)
             (!- g
                 '((fix (lambda (!)
                          (lambda (n)
                            (* n (! (sub1 n))))))
                   5)
                 q)))

(run! 1 q (!- '() '(cons (zero? 1) (zero? 0)) q))

(run! 1 q (!- '() '(cons (zero? 1) (cons (zero? 1) (zero? 0))) q))

(run! 1 t (!- '() `(lambda (x) (cons x x)) t))

(run! 1 t (!- '() `(lambda (x) (lambda (y) (cons (zero? x) (+ x y)))) t))

(run! 1 t (!- '() `(lambda (x) (zero? (car x))) t))

(run! 1 t (!- '() `((lambda (x) (zero? (car x))) (cons 0 1)) t))

(run! 1 t (!- '() `((lambda (x) (zero? (car x))) (cons 0 #f)) t))

(run! 1 t (!- '() `((lambda (x) (car x)) (cons (cons 0 0) #f)) t))

(run! 1 t (!- '() `((lambda (x) (zero? (car x))) (cons #f 0)) t))

(run! 1 t (!- '() `(lambda (x) (zero? (cdr x))) t))

(run! 1 t (!- '() `((lambda (x) (zero? (cdr x))) (cons 0 1)) t))

(run! 1 t (!- '() `((lambda (x) (zero? (cdr x))) (cons 0 #f)) t))

(run! 1 t (!- '() `((lambda (x) (zero? (cdr x))) (cons #f 0)) t))