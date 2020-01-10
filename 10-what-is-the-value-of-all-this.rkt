#lang racket

;; Example of entries
'((appetizer entrée beverage)
  (paté boeuf vin))

'((appetizer entrée beverage)
  (beer beer beer))

'((beverage dessert)
  ((food is) (number one with us)))

(define (new-entry x y) '(x y))

(define (lookup-in-entry-help name names values entry-f)
  (cond
   ((null? names) (entry-f))
   ((equal? name (first names)) (first values))
   (else (lookup-in-entry-help name (rest names) (rest values) entry-f)
	 )))

(define (lookup-in-entry name entry entry-f)
  (lookup-in-entry-help name (first entry) (second entry) entry-f))

(lookup-in-entry 'entrée '((appetizer entrée beverage) (food tastes good)) (lambda () false))

(lookup-in-entry 'dessert '((appetizer entrée beverage) (food tastes good)) (lambda () false))

;; Example of environments

'(((appetizer entrée beverage)
   (paté boeuf vin))
  '((beverage dessert)
    ((food is) (number one with us))))

(define (extend-table entry table) (cons entry table))

(define (lookup-in-table name table table-f)
  (cond
   ((null? table) false)
   ((lookup-in-entry name (first table) (lookup-in-table name (rest table) table-f)))))

(lookup-in-table 'entrée '(((entrée dessert) (spaghetti spumoni)) ((appetizer entrée beverage) (food tastes good)))
		 (lambda (name) name))

(define atom? (lambda (x) (and (not (pair? x)) (not (null? x)))))

(define (atom-to-action e)
  (cond
   ((number? e) '*const)
   ((or (eq? e #t) (eq? e #f)) '*const)
   ((or (eq? e 'cons) (eq? e 'car)
	(eq? e 'cdr) (eq? e 'null?)
	(eq? e 'eq?) (eq? e 'atom?)
	(eq? e 'zero?) (eq? e 'add1)
	(eq? e 'sub1)  (eq? e 'number?)) '*const)
   (else '*identifier)))

(define (list-to-action e)
  (cond
   ((null? e) '*quote)
   ((atom? (first e))
    (cond
     ((equal? (first e) 'quote) '*quote)
     ((equal? (first e) 'lambda) '*lambda)
     ((equal? (first e) 'cond) '*cond)
     (else '*application)))
   (else '*application)))

(define (expression-to-action e)
  (cond
   ((atom? e) (atom-to-action e))
   (else (list-to-action e))))

(define (build s1 s2) (cons s1 (cons s2 '())))

(define (value e) (meaning e '()))

(define (meaning e  table) ((expression-to-action e) e table))

(define (*const e table)
  (cond
   ((number? e) e)
   ((eq? e #t) #t)
   ((eq? e #f) #f)
   (else (build 'primitive e))))

(define (text-of e) (second e))

(define (*quote e table) (text-of e))

(define (*identifier e table)
  (lookup-in-table e table initial-table))

(define (initial-table name) (car '()))

(define (*lambda e table)
  (build 'primitive (cons table (cdr e))))

(define table-of first)
(define formals-of second)
(define body-of third)

(define (evcon lines table)
  (cond
   ((else? (question-of (first lines)))
    (meaning (answer-of (first lines)) table))
   ((meaning (question-of (first lines)) table) (meaning (answer-of (first lines)) table))
   (else (evcon (rest lines) table))))

(define (else? l) ((equal? (first l) 'else)))

(define question-of first)
(define answer-of second)

(define cond-lines-of rest)

(define (*cond e table)
  (evcon (cond-lines-of e) table))

(define (evlis args table)
  (cond
   ((null? args) '())
   (else (cons (meaning (first args) table)
	       (evlis (rest args) table)))))

(define (*application e table)
  (apply (meaning (function-of e) table)
	 (evlis (arguments-of e) table)))

(define function-of first)
(define arguments-of rest)

(define (primitive? l) (eq? (first l) 'primitive))
(define (non-primitive? l) (eq? (first l) 'non-primitive))

(define (apply fun vals)
  (cond
   ((primitive? fun) (apply-primitive (second fun) vals))
   ((non-primitive? fun) (apply-closure (second fun) vals))))

(define (:atom? x)
  (cond
   ((atom? x) true)
   ((null? x) false)
   ((eq? (first x) 'primitive) #t)
   ((eq? (first x) 'non-primitive) #t)
   (else #f)))

(define (apply-primitive name vals)
  (cond
   ((eq? name 'cons) (cons (first vals) (second vals)))
   ((eq? name 'car) (car (first vals)))
   ((eq? name 'cdr) (cdr (first vals)))
   ((eq? name 'null?) (null? (first vals)))
   ((eq? name 'eq?) (eq? (first vals) (second vals)))
   ((eq? name 'atom?) (:atom? (first vals)))
   ((eq? name 'zero?) (zero? (first vals)))
   ((eq? name 'add1) (add1 (first vals)))
   ((eq? name 'sub1) (sub1 (first vals)))
   ((eq? name 'number?) (number? (first vals)))))

(define (apply-closure closure vals)
  (meaning (body-of closure)
	   (extend-table (new-entry (formals-of closure) vals)
			 (table-of closure))))
