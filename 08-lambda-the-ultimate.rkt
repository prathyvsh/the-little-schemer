#lang racket

(define (= n m)
  (cond
   ((and (zero? m) (zero? n)) #t)
   ((zero? m) #f)
   ((zero? n) #f)
   (else (= (sub1 m) (sub1 n)))))

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
   ((and (atom? l1) (atom? l2)) (eqan? l1 l2))
   ((or (atom? l1) (atom? l2)) false)
   ((and (atom? (first l1)) (atom? (first l2)))
    (and (eqan? (first l1) (first l2)) (eqlist? (rest l1) (rest l2))))
   (else (and (eqlist? (first l1) (first l2))
	      (eqlist? (rest l1) (rest l2))))))

(define (equal? s1 s2)
  (cond
   ((and (atom? s1)
	 (atom? s2))
    (eqan? s1 s2))
   ((or (atom? s1)
	(atom? s2)) #f)
   (else (eqlist? s1 s2))))

(define (rember s l)
  (cond
   ((null? l) '())
   ((equal? s (first l)) (rest l))
   (else (cons (first l) (rember s (rest l))))))


(define (rember-f f a l)
  (cond
   ((null? l) '())
   ((f a (first l)) (rember-f f a (rest l)))
   (else (cons (first l) (rember-f f a (rest l))))))

(equal? (rember-f = 5 '(6 2 5 3)) '(6 2 3))
(equal? (rember-f eq? 'jelly '(jelly beans are good)) '(beans are good))
(equal? (rember-f equal? '(pop corn) '(lemonade (pop corn) and (cake))) '(lemonade and (cake)))

(define (eq?-c a) (lambda (x) (eq? x a)))

(equal? ((eq?-c 'salad) 'hi) false)
(equal? ((eq?-c 'salad) 'salad) true)

(define eq?-salad (eq?-c 'salad))

(equal? (eq?-salad 'salad) true)
(equal? (eq?-salad 'tuna) false)

(define (rember-f2 f)
  (lambda (a l)
    (cond
     ((null? l) '())
     ((f a (first l)) (rember-f f a (rest l)))
     (else (cons (first l) ((rember-f2 f) a (rest l)))))))

(define rember-eq? (rember-f2 eq?))

(equal? (rember-eq? 'tuna '(tuna salad tuna is good)) '(salad is good))
(equal? (rember-eq? 'tuna '(shrimp salad and tuna salad)) '(shrimp salad and salad))
(equal? ((rember-f2 eq?) 'eq? '(equal? eq? eqan? eqlist? eqpair)) '(equal? eqan? eqlist? eqpair))


(define (insertL-f f)
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((f (first lat) old) (cons new lat))
     (else (cons (first lat) ((insertL-f f) new old (rest lat)))))))

(equal? ((insertL-f eq?) 'hello 'there '(there Matt)) '(hello there Matt))

(define (insertR-f f)
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((f (first lat) old) (cons (first lat) (cons new (rest lat))))
     (else (cons (first lat) ((insertR-f f) new old (rest lat)))))))

(equal? ((insertR-f eq?) 'there 'hello '(hello Matt)) '(hello there Matt))

(define (seqL new old l) (cons new (cons old l)))
(define (seqR new old l) (cons old (cons new l)))

(define (insert-g seq)
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? (first lat) old) (seq new old (rest lat)))
     (else (cons (first lat) (insert-g new old (rest lat)))))))

(define insertL (insert-g seqL))

(define insertR (insert-g seqR))

(define insertL2 (insert-g (lambda (new old l) (cons new (cons old l)))))

(define (subst new old lat)
  (cond
   ((null? lat) '())
   ((eq? (first lat) old) (cons new (rest lat)))
   (else (cons (first lat) (subst new old (rest lat))))))


(define (seqS new old l) (cons new l))

(define subst2 (insert-g seqS))

(define (seqrem new old l) l)

;; #f plays the role of signifying that the value will be deleted.
(define (rember2 a l) ((insert-g seqrem) #f a l))

(define (* n m)
  (cond
   ((zero? m) 0)
   (else (+ n (* n (sub1 m))))))

(define (^ n m)
  (cond
   ((zero? m) 1)
   (else (* n (^ n (sub1 m))))))


(define (atom-to-function x)
  (cond
   ((eq? x '+) +)
   ((eq? x '*) *)
   (else ^)))

(define (1st-sub-exp aexp) (first aexp))

(define (operator aexp) (first (rest aexp)))

(define (2nd-sub-exp aexp) (first (rest (rest aexp))))

(define (value exp)
  (cond
   ((atom? exp) exp)
   ((null? exp) '())
   ((list? exp) ((atom-to-function (operator exp)) (value (1st-sub-exp exp))
		 (value (2nd-sub-exp exp))))
   (else false)))

(eq? (value '13) 13)
(eq? (value '(1 + 3)) 4)
(eq? (value '(1 + (3 ^ 4))) 82)

(define (multirember-f f)
  (lambda (a lat)
    (cond
     ((null? lat) '())
     ((f a (first lat)) ((multirember-f f) a (rest lat)))
     (else (cons (first lat) ((multirember-f f) a (rest lat)))))))

(equal? ((multirember-f eq?) 'tuna '(shrimp salad tuna salad and tuna)) '(shrimp salad salad and))

(define multirember-eq? (multirember-f eq?))

(define eq?-tuna (eq?-c 'tuna))

(define (multiremberT f lat)
  (cond
   ((null? lat) '())
   ((f (first lat)) (multiremberT f (rest lat)))
   (else (cons (first lat) (multiremberT f (rest lat))))))

(equal? (multiremberT eq?-tuna '(shrimp salad tuna salad and tuna)) '(shrimp salad salad and))

(define (multirember&co a lat col)
  (cond
   ((null? lat) (col '() '()))
   ((eq? (first lat) a) (multirember&co a (rest lat) (lambda (newlat seen) (col newlat (cons (first lat) seen)))))
   (else (multirember&co a (rest lat) (lambda (newlat seen) (col (cons (first lat) newlat) seen))))))

(define (a-friend x y) (null? y))

(define (multiinsertL new old lat)
  (cond
   ((null? lat) '())
   ((equal? (first lat) old) (cons new (cons old (multiinsertL new old (rest  lat)))))
   (else (cons (first lat) (multiinsertL new old (rest lat))))))


(define (multiinsertR new old lat)
  (cond
   ((null? lat) '())
   ((equal? (first lat) old) (cons old (cons new (multiinsertL new old (rest  lat)))))
   (else (cons (first lat) (multiinsertL new old (rest lat))))))

(define (multiinsertLR new oldL oldR lat)
  (cond
   ((null? lat) '())
   ((eq? (first lat) oldL) (cons new (cons oldL (multiinsertLR new oldL oldR (rest lat)))))
   ((eq? (first lat) oldR) (cons oldR (cons new (multiinsertLR new oldL oldR (rest lat)))))
   (else (cons (first lat) (multiinsertLR new oldL oldR (rest lat))))))

(equal? (multiinsertLR 'apple false 'grapes '(grapes watermelon orange lemon))
	'(grapes apple watermelon orange lemon))

(equal? (multiinsertLR 'apple 'grapes false '(grapes watermelon orange lemon))
	'(apple grapes watermelon orange lemon))

(equal? (multiinsertLR 'apple 'grapes 'lemon '(grapes watermelon orange lemon))
	'(apple grapes watermelon orange lemon apple))

(define (multiinsertLR&co new oldL oldR lat col)
  (cond
   ((null? lat) (col '() 0 0))
   ((eq? (first lat) oldL) (multiinsertLR&co new oldL oldR (rest lat) (lambda (newlat L R) (col (cons new (cons oldL newlat)) (add1 L) R))))
   ((eq? (first lat) oldR) (multiinsertLR&co new oldL oldR (rest lat) (lambda (newlat L R) (col (cons oldR (cons new newlat)) L (add1 R)))))
   (else (multiinsertLR&co new oldL oldR (rest lat) (lambda (newlat L R) (col (cons (first lat) newlat) L R))))))

(equal? (multiinsertLR&co 'salty 'fish 'chips '(chips and fish or fish and chips) (lambda (x y z) x))
	'(chips salty and salty fish or salty fish and chips salty))

(equal? (multiinsertLR&co 'salty 'fish 'chips '(chips and fish or fish and chips) (lambda (x y z) (+ y z))) 4)

(define (quotient n m)
  (cond
   ((< n m) 0)
   (else (add1 (quotient (- n m) m)))))

(define (even? n) (= (* (quotient n 2) 2) n))

(equal? (even? 2) true)
(equal? (even? 3) false)
(equal? (even? 40) true)
(equal? (even? 41) false)

(define (evens-only* loe)
  (cond
   ((null? loe) '())
   ((atom? (first loe)) (cond ((even? (first loe)) (cons (first loe) (evens-only* (rest loe))))
			      (else (evens-only* (rest loe)))))
   (else (cons (evens-only* (first loe)) (evens-only* (rest loe))))))

(equal? (evens-only* '((9 1 2 8) 3 10 ((9 9) 7 6) 2)) '((2 8) 10 (() 6) 2))


(define (evens-only*&co loe col)
  (cond
   ((null? loe) (col '() 1 0))
   ((atom? (first loe)) (cond
			 ((even? (first loe)) (evens-only*&co (rest loe)
							      (lambda (newl p s) (col (cons (first loe) newl)
										      (* (first loe) p) s))))
			 (else (evens-only*&co (rest loe) (lambda (newl p s) (col newl p (+ (first loe) s)))))))
   (else (evens-only*&co (first loe) (lambda (newl p s) (evens-only*&co (rest loe)
									(lambda (anewl ap as)
									  (col (cons newl anewl) (* p ap) (+ s as)))))))))

(equal? (evens-only*&co '((9 1 2 8) 3 10 ((9 9) 7 6) 2) (lambda (l p s) l)) '((2 8) 10 (() 6) 2))

(define (the-last-friend newl product sum) (cons sum (cons product newl)))

(equal? (evens-only*&co '((9 1 2 8) 3 10 ((9 9) 7 6) 2) the-last-friend) '(38 1920 (2 8) 10 (() 6) 2))
