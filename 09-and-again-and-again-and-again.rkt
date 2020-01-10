#lang racket

(define (pick a lat)
  (cond
   ((null? lat) lat)
   ((= a 1) (first lat))
   (else (pick (sub1 a) (rest lat)))))

(define (looking a lat)
  (keep-looking a (pick 1 lat) lat))

(define (keep-looking a idx lat)
  (cond
   ((number? (pick idx lat)) (keep-looking a (pick idx lat) lat))
   (else (equal? a (pick idx lat)))))

(equal? (keep-looking 'caviar 3 '(6 2 4 caviar 5 7 3)) true)

(equal? (looking 'caviar '(6 2 4 caviar 5 7 3)) true)

(equal? (looking 'caviar '(6 2 grits caviar 5 7 3)) false)

(define (eternity x) (eternity x))

(define (build s1 s2) (cons s1 (cons s2 '())))

(define (atom? x) (or (symbol? x) (number? x) (boolean? x)))

(define (shift pair)
  (build (first (first pair))
	 (build (second (first pair))
		(second pair))))

(equal? (shift '((a b) c)) '(a (b c)))

(equal? (shift '((a b) (c d))) '(a (b (c d))))

(define (a-pair? a)
  (cond
   ((atom? a) false)
   ((null? a) false)
   ((null? (rest a)) false)
   ((null? (rest (rest a))) true)
   (else false)))

(define (align pora)
  (cond
   ((atom? pora) pora)
   ((a-pair? (first pora)) (align (shift pora)))
   (else (build (first pora) (align (second pora))))))

(align '(a b))

(define (length* pora)
  (cond
   ((atom? pora) 1)
   (else (+ (length* (first pora)) (length* (rest pora))))))

(define (weight* pora)
  (cond
   ((atom? pora) 1)
   (else (+ (* (weight* (first pora)) 2)
	    (weight* (second pora))))))

(define (revpair pair) (build (second pair) (first pair)))

(define (shuffle pora)
  (cond
   ((atom? pora) pora)
   ((a-pair? (first pora)) (shuffle (revpair pora)))
   (else (build (first pora) (shuffle (second pora))))))

(equal? (shuffle '(a (b c))) '(a (b c)))

(define (C n)
  (cond
   ((= n 1) 1)
   ((even? n) (C (/ n 2)))
   (else (C (add1 (* 3 n))))))

(define (A n  m)
  (cond
   ((zero? n) (add1 m))
   ((zero? m) (A (sub1 n) 1))
   (else (A (sub1 n) (A n (sub1 m))))))

;; Is said to be undefinable
(define (will-stop? x) x)

(define (last-try x)
  (and (will-stop? last-try) (eternity x)))

(define (length0 l)
  (cond
   ((null? l) 0)
   (else (add1 (eternity (cdr l))))))

(define (length<=1 l)
  (cond
   ((null? l) 0)
   (else (add1 (length0 (cdr l))))))

(define (length<=2 l)
  (cond
   ((null? l) 0)
   (else (add1 (length<=1 l)))))

((lambda (length)
   (lambda (l)
     (cond
      ((null? l) 0)
      (else (add1 (length (cdr l))))))) eternity)

((lambda (f) (lambda (l)
	       (cond
		((null? l) 0)
		(else (add1 (f (cdr l)))))))
 (lambda (g) (lambda (l)
	       (cond
		((null? l) 0)
		(else (add1 (g  (cdr l)))))) eternity))
