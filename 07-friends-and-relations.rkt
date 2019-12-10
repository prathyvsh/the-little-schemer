#lang racket

(define (atom? x) (or (symbol? x) (number? x) (boolean? x)))

(define (eqan? a1 a2)
  (cond
   ((and (number? a1) (number? a2)) (= a1 a2))
   ((or (number? a1) (number? a2)) #f)
   (else (eq? a1 a2))))

(define (eqlist? l1 l2)
  (cond
   ((and (null? l1) (null? l2)) #t)
   ((or (null? l1) (null? l2)) #f)
   ((and (atom? l1) (null? l2)) #f)
   ((and (null? l1) (atom? l2)) #f)
   ((and (atom? l1) (atom? l2)) (eqan? l1 l2))
   ((and (atom? (first l1)) (atom? (first l2)))
    (and (eq? (first l1) (first l2)) (eqlist? (rest l1) (rest l2))))
   (else (and (eqlist? (first l1) (first l2))
        (eqlist? (rest l1)
                 (rest l2))))))

(define (equal? s1 s2)
  (cond
   ((and (atom? s1)
         (atom? s2))
    (eqan? s1 s2))
   ((or (atom? s1)
        (atom? s2)) #f)
   (else (eqlist? s1 s2))))

(define member?
  (lambda (a lat)
  (cond
  ((null? lat) #f)
  (else (or (equal? a (first lat))
            (member? a (rest lat)))))))
    

(define (set? s)
  (cond
    ((null? s) #t)
    (else (and (not (member? (first s) (rest s))) (set? (rest s))))))

(eq? (set? '(apple peaches apple plum)) #f)

(eq? (set? '(apple peaches pears plums)) #t)

(eq? (set? '(apple 3 pear 4 9 apple 3 4)) #f)

(define (makeset lat)
  (cond
  ((null? lat) '())
  ((member? (first lat) (rest lat)) (makeset (rest lat)))
  (else (cons (first lat) (makeset (rest lat))))))

(equal? (makeset '(apple peach pear peach plum apple lemon peach)) '(pear plum apple lemon peach))

(define (multirember a lat)
  (cond
   ((null? lat) '())
   ((equal? a (first lat)) (multirember a (rest lat)))
   (else (cons (first lat) (multirember a (rest lat))))))

(define (makeset2 lat)
(cond
  ((null? lat) '())
  (else (cons (first lat) (makeset2 (multirember (first lat) (rest lat)))))))

(equal? (makeset2 '(apple peach pear peach plum apple lemon peach)) '(apple peach pear plum lemon))

(equal? (makeset2 '(apple 3 pear 4 9 apple 3 4)) '(apple 3 pear 4 9))

(define (subset? set1 set2)
  (cond
    ((and (null? set1) (null? set2)) #t)
    ((null? set1) #t)
    ((null? set2) #f)
    (else (and (member? (first set1) set2) (subset? (rest set1) set2)))))

(eq? (subset? '(5 chicken wings) '(5 hamburgers 2 pieces fried chicken and light duckling wings)) #t)

(eq? (subset? '(4 pounds of horseradish) '(four pounds chicken and 5 ounces horseradish)) #f)

(define (eqset? set1 set2) (and (subset? set1 set2) (subset? set2 set1)))

(define (intersect? set1 set2)
  (cond
    ((or (null? set1) (null? set2)) '())
    (else (or (member? (first set1) set2) (intersect? (rest set1) set2)))))

(eq? (intersect? '(stewed tomatoes and macaroni) '(macaroni and cheese)) #t)

(define (intersect set1 set2)
  (cond
    ((or (null? set1) (null? set2)) '())
    ((member? (first set1) set2) (cons (first set1) (intersect (rest set1) set2)))
    (else (intersect (rest set1) set2))))

(equal? (intersect '(stewed tomatoes and macaroni) '(macaroni and cheese)) '(and macaroni))

(define (union set1 set2)
  (cond
    ((null? set1) set2)
    ((member? (first set1) set2) (union (rest set1) set2))
    (else (cons (first set1) (union (rest set1) set2)))))

(equal? (union '(stewed tomatoes and macaroni casserole) '(macaroni and cheese)) '(stewed tomatoes casserole macaroni and cheese))

(define (difference set1 set2)
  (cond
    ((null? set1) '())
    ((member? (first set1) set2) (union (rest set1) set2))
    (else (cons (first set1) (union (rest set1) set2)))))

(define (intersectall l-set)
  (cond
    ((null? (rest l-set)) (first l-set))
    (else (intersect (first l-set) (intersectall (rest l-set))))))

(equal? (intersectall '((6 pears and) (3 peaches and 6 peppers) (8 pears and 6 plums) (and 6 prunes with some apples))) '(6 and))

(define (a-pair? a)
  (cond
    ((atom? a) false)
    ((null? a) false)
    ((null? (rest a)) false)
    ((null? (rest (rest a))) true)
    (else false)))

(eq? (a-pair? '(pear pear)) true)
(eq? (a-pair? '(3 7)) true)
(eq? (a-pair? '((2) (pair))) true)
(eq? (a-pair? '(full (house))) true)

(define (second p) (car (cdr p)))

(define (build s1 s2) (cons s1 (cons s2 '())))

(define (third p) (car (cdr (cdr p))))

(define (rel? s)
  (and (set? s)
       (cond
         ((null? s) true)
        (else (and (a-pair? (first s)) (rel? (rest s)))))))

(eq? (rel? '(apple peaches pumpkin pie)) false)
(eq? (rel? '((apples peaches) (pumpkin pie) (apples peaches))) false)
;; (eq? (rel? '((apples peaches) (pumpkin pie))) false)
;; (eq? (rel? '((4 3) (4 2) (7 6) (6 2) (3 4))) true)

(define (get-first l)
  (cond
   ((null? l) '())
   (else (first l))))

(define (firsts ls)
  (cond
  ((null? ls) '())
  (else (cons (get-first (first ls)) (firsts (rest ls))))))

(define (fun? s) (set? (firsts s)))

(equal? (fun? '((4 3) (4 2) (7 6) (6 2) (3 4))) false)

(equal? (fun? '((8 3) (4 2) (7 6) (6 2) (3 4))) true)

(equal? (fun? '((d 4) (b 0) (b 9) (e 5) (g 4))) false)

(define (revrel rel)
  (cond
    ((null? rel) '())
    (else (cons (build (second (first rel)) (first (first rel)))
                (revrel (rest rel))))))

(equal? (revrel '((8 a) (pumpkin pie) (got sick))) '((a 8) (pie pumpkin) (sick got)))

(define (revpair pair) (build (second pair) (first pair)))

(define (revrel2 rel)
  (cond
    ((null? rel) '())
    (else (cons (revpair (first rel))
                (revrel (rest rel))))))

(equal? (revrel2 '((8 a) (pumpkin pie) (got sick))) '((a 8) (pie pumpkin) (sick got)))

(define (seconds fun)
  (cond
    ((null? fun) '())
    (else (cons (second (first fun)) (seconds (rest fun))))))

(define (fullfun? fun) (and (set? (firsts fun)) (set? (seconds fun))))

(equal? (fullfun? '((8 3) (4 2) (7 6) (6 2) (3 4))) false)
(equal? (fullfun? '((8 3) (4 8) (7 6) (6 2) (3 4))) true)
(equal? (fullfun? '((grape raisin) (plum prune) (stewed prune))) false)

(define (one-to-one? fun) (and (fun? fun) (fun? (revrel fun))))

(equal? (one-to-one? '((chocolate chip) (doughy cookie))) true)