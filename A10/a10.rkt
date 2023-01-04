#lang racket
(require "mk.rkt")
(require "numbers.rkt")

;;; Q1
#; (run! 2 (q)
         (== 5 q)
         (conde
          [(conde
            [(== 5 q)
             (== 6 q)])
           (== 5 q)]
          [(== q 5)]))

;;; Ans: The value is 5
;;; First, q is unified with 5.
;;; Then there are 2 conde, the first conde has 2 clauses and 2nd one also has 2 clauses.
;;; The 1st clause in 1st conde is another conde, this clause fails and q in only equal to 5
;;; and not 6. It then moves to next clause which is true, but has no evaluation. Hence q is
;;; bound to 5.


;;; Q2
#; (run! 1 (q)
         (fresh (a b)
                (== `(,a ,b) q)
                (absento 'tag q)
                (symbolo a)))

;;; Ans: (_0 _1)(=/= _1 tag) (=/= _0 tag) (absento _0 (tag)) (absento _1 (tag))
;;; first 2 fresh variables a and b are declared
;;; next q is matched with tag list with a and b as its elements.
;;; next goal is to make sure no tags are present inside the tag list
;;; in other words a and b are not tags.
;;; last goal binds a to be a symbol only.
;;; therefore we get the above mentioned result.


;;; Q3
;;; What do the following miniKanren constraints mean?

;;; 1. == : EQUALITY CONSTRAINT: it takes in 2 arguements and unifies (or associates)
;;; the variable (2nd arguement) with the value (1st arguement)

;;; 2. =/= : DISEQUALITY CONSTRAINT: it takes in 2 arguements and checks if
;;; the variable (2nd arguement) is not equal to the given value (1st arguement).
;;; In other words it unifies (or associates) the variable (2nd arguement) with
;;; every value other than the given value (1st arguement)

;;; 3. numbero : it takes in 1 arguements and checks if the variable is of number type

;;; 4. symbolo : it takes in 1 arguement and checks if the variable is of symbol type

;;; 5. absento : it takes in 2 arguements and checks if the type (1st arguement) is
;;; absent in given variable (2nd arguement)


;;; Q4

;;; (define assoc
;;;   (lambda (x ls)
;;;     (match-let* ((`(,a . ,d) ls)
;;;                  (`(,aa . ,da) a))
;;;       (cond
;;;         ((equal? aa x) a)
;;;         ((not (equal? aa x)) (assoc x d))))))

(defrel (assoco x ls o)
  (fresh (a d aa da)
         (== `(,a . ,d) ls)
         (== `(,aa . ,da) a)
         (conde
          [(== aa x) (== o a)]
          [(=/= aa x)
           (assoco x d o)])))

(printf "-----------------------START ASSOCO-----------------------\n")
(run! 5 q (assoco 'x '() q))
(run! 5 q (assoco 'x '((x . 5)) q))
(run! 5 q (assoco 'x '((y . 6) (x . 5)) q))
(run! 5 q (assoco 'x '((x . 6) (x . 5)) q))
(run! 5 q (assoco 'x '((x . 5)) '(x . 5)))
(run! 5 q (assoco 'x '((x . 6) (x . 5)) '(x . 6)))
(run! 5 q (assoco 'x '((x . 6) (x . 5)) '(x . 5)))
(run! 5 q (assoco q '((x . 6) (x . 5)) '(x . 5)))
(run! 5 q (assoco 'x '((x . 6) . ,q) '(x . 6)))
(run! 5 q (assoco 'x q '(x . 5)))
(run! 5 q (fresh (x y z)
                 (assoco x y z)
                 (== `(,x ,y ,z) q)))
(printf "-----------------------END ASSOCO-----------------------\n\n\n")

;;; (define reverse
;;;   (lambda (ls)
;;;     (cond
;;;       ((equal? '() ls) '())
;;;       (else
;;;        (match-let* ((`(,a . ,d) ls)
;;;                     (res (reverse d)))
;;;          (append res `(,a)))))))

(defrel (listo ls o)
  (== (list ls) o))

(defrel (reverseo ls o)
  (conde
   [(== '() ls) (== o '())]
   [(fresh (a d temp res)
           (== `(,a . ,d) ls)
           (reverseo d temp)
           (listo a res)
           (appendo temp res o))]))

(printf "-----------------------START REVERSO-----------------------\n")
(run! 1 q (reverseo '(a) q))
(run! 1 q (reverseo '(a b c d) q))
(run! 1 q (fresh (x) (reverseo `(a b ,x c d) q)))
(run! 1 x (reverseo `(a b ,x d) '(d c b a)))
(run! 1 x (reverseo `(a b c d) `(d . ,x)))
(run! 1 q (fresh (x) (reverseo `(a b c d) `(d . (,q . ,x)))))
(run! 10 q (fresh (x y) (reverseo x y) (== `(,x ,y) q)))
(printf "-----------------------END REVERSO-----------------------\n\n\n")


;;; (define stutter
;;;   (lambda (ls)
;;;     (cond
;;;       ((equal? '() ls) '())
;;;       (else
;;;        (match-let* ((`(,a . ,d) ls)
;;;                     		     (res (stutter d)))
;;;          `(,a ,a . ,res))))))

(defrel (stuttero ls o)
  (conde
   [(== '() ls) (== o '())]
   [(fresh (a d res)
           (== `(,a . ,d) ls)
           (== o `(,a ,a . ,res))
           (stuttero d res))]))

(printf "-----------------------START STUTTERO-----------------------\n")
(run! 1 q (stuttero q '(1 1 2 2 3 3)))
(run! 20 q (stuttero q '(1 1 2 2 3 3)))
(run! 1 q (fresh (a b c d) (== q `(,a ,b ,c ,d)) (stuttero a `(1 ,b ,c 2 3 ,d))))
(run! 1 q (fresh (a b c d) (== q `(,a ,b ,c ,d)) (stuttero `(,b 1) `(,c . ,d))))
(printf "-----------------------END STUTTERO-----------------------\n\n\n")

;;; Q5 BRAINTEASER

(defrel (lengtho ls o)
  (conde
   [(== '() ls) (== o '())]
   [(fresh (a d res)
           (== `(,a . ,d) ls)
           (lengtho d res)
           (pluso '(1) res o))]))

(printf "-----------------------START BRAINTEASER-----------------------\n")
(run! 1 q (lengtho '() q))
(run! 1 q (lengtho '(a b) q))
(run! 1 q (lengtho '(a b c) q))
(run! 1 q (lengtho '(a b c d e f g) q))
(run! 1 q (lengtho q (build-num 0)))
(run! 1 q (lengtho q (build-num 5)))
(run! 10 q (fresh (x y) (lengtho x y) (== `(,x ,y) q)))
(printf "-----------------------END BRAINTEASER-----------------------\n\n\n")