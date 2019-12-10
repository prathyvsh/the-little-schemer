#lang racket

(define (atom? x) (or (symbol? x) (number? x)))

;; Helper function to check if two lists are equal
(define (listeq? a b)
  (cond
    ((and (null? a) (null? b)) true)
    ((or (null? a) (null? b)) false)
    (else (cond
            ((and (list? (first a)) (list? (first b)))
             (and (listeq? (first a) (first b))
                  (listeq? (rest a) (rest b))))
             ((eq? (first a) (first b)) (listeq? (rest a) (rest b)))
            (else false)))))

;; My definition
(define (rember* a l)
  (cond
   ((null? l) '())
   ((atom? (first l)) (cond
                      ((eq? (first l) a) (rember* a (rest l)))
                      (else (cons (first l) (rember* a (rest l))))))
   (else (cons (rember* a (first l)) (rember* a (rest l))))))

(listeq? (rember* 'cup '((coffee) cup ((tea) cup) (and (hick)) cup))
	 '((coffee) ((tea)) (and (hick))))

(listeq? (rember* 'sauce '(((tomato sauce))
			   ((bean) sauce)
			   (and ((flying)) sauce)))
	 (((tomato)) ((bean)) (and ((flying)))))

;; My definition
(define (insertR* new old l)
  (cond
   ((null? l) '())
   ((atom? (first l)) (cond
               ((eq? (first l) old) (cons old (cons new (insertR* new old (rest l)))))
               (else (cons (first l) (insertR* new old (rest l))))))
   (else (cons (insertR* new old (first l))
               (insertR* new old (rest l))))))

(insertR* 'roast 'chuck '((how much (wood))
                          could
                          ((a (wood) chuck))
                          (((chuck)))
                          (if (a) ((wood chuck)))
                          could chuck wood))

;; => '((how much (wood)) could ((a (wood) chuck roast)) (((chuck roast))) (if (a) ((wood chuck roast))) could chuck roast wood))

;; My definition
(define (occur* a l)
  (cond
   ((null? l) 0)
   ((atom? (first l)) (cond
                       ((eq? (first l) a) (add1 (occur* a (rest l))))
                       (else (occur* a (rest l)))))
   (else (+ (occur* a (first l))
            (occur* a (rest l))))))

(occur* 'banana '((banana)
                  (split ((((banana ice)))
                          (cream (banana))
                          sherbet))
                  (banana)
                  (bread)
                  (banana brandy)))
;; => 5

;; My definition
(define (subst* new old l)
  (cond
   ((null? l) '())
   ((atom? (first l)) (cond
                       ((eq? (first l) old) (cons new (subst* new old (rest l))))
                       (else (cons (first l) (subst* new old (rest l))))))
   (else (cons (subst* new old (first l))
               (subst* new old (rest l))))))

(subst* 'orange 'banana '((banana)
                          (split ((((banana ice)))
                                  (cream (banana))
                                  sherbet))
                          (banana)
                          (bread)
                          (banana brandy)))
;; '((orange) (split ((((orange ice))) (cream (orange)) sherbet)) (orange) (bread) (orange brandy))

;; My definition
(define (insertL* new old l)
  (cond
   ((null? l) '())
   ((atom? (first l)) (cond
               ((eq? (first l) old) (cons new (cons old (insertL* new old (rest l)))))
               (else (cons (first l) (insertL* new old (rest l))))))
   (else (cons (insertL* new old (first l))
               (insertL* new old (rest l))))))

(insertL* 'pecker 'chuck '((how much (wood))
                          could
                          ((a (wood) chuck))
                          (((chuck)))
                          (if (a) ((wood chuck)))
                          could chuck wood))

;; '((how much (wood)) could ((a (wood) pecker chuck)) (((pecker chuck))) (if (a) ((wood pecker chuck))) could pecker chuck wood)

;; My definition
(define (member* a l)
  (cond
   ((null? l) #f)
   ((atom? (first l)) (or (eq? (first l) old) (member* a (rest l))))
   (else (or (member* a (first l))
             (member* a (rest l))))))

(member* 'chips '((potato) (chips ((with) fish) (chips))))
;; => #t

(define (leftmost l)
  (cond
   ((atom? (first l)) (first l))
   (else (leftmost (first l)))))

(leftmost '((potato) (chips ((with) fish) chips)))
;; => potato

(leftmost '(((hot) (tuna (and))) cheese))
;; => hot

(define x 'pizza)

(define l '(mozzarella pizza))

(and (atom? (first l))
     (eq? (first l) x))
;; => #f

(define x 'pizza)

(define l '(pizza pizza))

(and (atom? (first l))
     (eq? (first l) x))
;; => #t

;; My definition
(define (eqlist? l1 l2)
  (cond
   ((and (null? l1) (null? l2)) #t)
   ((and (atom? l1) (atom? l2)) (eqan? l1 l2))
   ((or (atom? l1) (atom? l2)) false)
   ((and (atom? (first l1)) (atom? (first l2)))
    (and (eqan? (first l1) (first l2)) (eqlist? (rest l1) (rest l2))))
   (else (and (eqlist? (first l1) (first l2))
	      (eqlist? (rest l1) (rest l2))))))
	 
(eqlist? '(strawberry ice cream)
         '(strawberry ice cream))
;; => #t

(eqlist? '(strawberry ice cream)
         '(strawberry cream ice))
;; => #f

(eqlist? '(banana ((split)))
         '((banana) (split)))
;; => #f


(eqlist? '(beef ((sausage)) (and (soda)))
         '(beef ((salami)) (and (soda))))
;; => #f

(eqlist? '(beef ((sausage)) (and (soda)))
        '(beef ((sausage)) (and (soda))))
;; => #t

(eqlist? '(banana (()) () (split))
         '(banana (()) () (split ())))
;; => #f

(eqlist? '(banana (()) () (split))
         '(banana (()) () (split)))
;; => #t

;; An S-exp is either an atom or a list of S-expressions.

(define (equal? s1 s2)
  (cond
   ((and (atom? s1)
         (atom? s2))
    (eq? s1 s2))
   ((or (atom? s1)
        (atom? s2)) #f)
   (else (eqlist? s1 s2))))

(define (eqlist? l1 l2)
  (cond
   ((and (null? l1) (null? l2)) #t)
   ((or (null? l1) (null? l2)) #f)
   (else (and (equal? (first l1) (first l2))
        (eqlist? (rest l1)
                 (rest l2))))))

(eqlist? '(strawberry ice cream)
         '(strawberry ice cream))
;; => #t

(eqlist? '(strawberry ice cream)
         '(strawberry cream ice))
;; => #f

(eqlist? '(banana ((split)))
         '((banana) (split)))
;; => #f


(eqlist? '(beef ((sausage)) (and (soda)))
         '(beef ((salami)) (and (soda))))
;; => #f

(eqlist? '(beef ((sausage)) (and (soda)))
        '(beef ((sausage)) (and (soda))))
;; => #t

(eqlist? '(banana (()) () (split))
         '(banana (()) () (split ())))
;; => #f

(eqlist? '(banana (()) () (split))
         '(banana (()) () (split)))
;; => #t

;; Rember with equal?
(define (rember s l)
  (cond
   ((null? l) '())
   ((equal? s (first l)) (rest l))
   (else (cons (first l) (rember s (rest l))))))

(rember 'potato '(potato salad))
;; => '(salad)

;; Design the functions correctly. Then think about
;; connecting them or using them together.

;; We can start using equal? for all function except
;; for the functions that made possible the construction
;; of equal? in the first place such as eqan?.

;; This was a pretty good chapter. This is probably one of the
;; the best ROIs for a beginner encountering recursion. But from
;; the perspective of someone who has finished HTDP before,
;; this is an exercise in re-inforcing the learnings.
