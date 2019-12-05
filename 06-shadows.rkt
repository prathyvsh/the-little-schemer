#lang racket

(eq? (eq? (quote a) 'a) true)

(eq? (eq? 'a 'a) true)

(define atom? (lambda (x) (and (not (pair? x)) (not (null? x)))))

(define (op? m)
  (cond
  ((null? m) true)
  (else (and (or (eq? (first m) '+) (eq? (first m) '*) (eq? (first m) '^)) (numbered1? (rest m))))))

;; My definition
(define (numbered1? m)
  (cond
   ((null? m) true)
   ((atom? m) (number? m))
   ((atom? (first m)) (and (numbered1? (first m)) (op? (rest m))))
   ((list? (first m)) (and (numbered1? (first m)) (op? (rest m))))
   (else false)))

(eq? (numbered1? 1) true)

(eq? (numbered1? '(3 + (4 ^ 5))) true)

(eq? (numbered1? '(2 * sausage)) false)

;; A simplified version is provided as:
(define (numbered? aexp)
  (cond
   ((atom? aexp) (number? aexp))
   (else (and (numbered? (first aexp))
              (numbered? (first (rest (rest aexp))))))))

;; This function doesn't work on some corner cases like:
;; '() and has is based on an assumption that (first (rest aexp))
;; will be a defined operation. some other cases but is operationally
;; the same as above version and more elegant.

(define (zero? n) (= n 0))

(define (+ n m)
  (cond
   ((zero? m) n)
   (else (add1 (+ n (sub1 m))))))


(define (* n m)
  (cond
  ((zero? m) 0)
  (else (+ n (* n (sub1 m))))))

(define (^ n m)
  (cond
   ((zero? m) 1)
   (else (* n (^ n (sub1 m))))))

(define (value exp)
  (cond
    ((atom? exp) exp)
    ((list? exp)
     (cond
       ((eq? (first (rest exp)) '+)
           (+ (value (first exp)) (value (first (rest (rest exp))))))
       ((eq? (first (rest exp)) '*)
           (* (value (first exp)) (value (first (rest (rest exp))))))
       ((eq? (first (rest exp)) '^)
           (^ (value (first exp)) (value (first (rest (rest exp))))))
       (else #f)))
    (else #f)))

(eq? (value '13) 13)
(eq? (value '(1 + 3)) 4)
(eq? (value '(1 + (3 ^ 4))) 82)


(define (1st-sub-exp aexp) (first (rest aexp)))

(define (2nd-sub-exp aexp) (first (rest (rest aexp))))

(define (operator aexp) (first aexp))

;; Computes value of the s-expression
(define (sexpr-value exp)
  (cond
    ((atom? exp) exp)
    ((list? exp)
     (cond
       ((eq? (operator exp) '+)
           (+ (sexpr-value (1st-sub-exp exp)) (sexpr-value (2nd-sub-exp exp))))
       ((eq? (operator exp) '*)
           (* (sexpr-value (1st-sub-exp exp)) (sexpr-value (2nd-sub-exp exp))))
       ((eq? (operator exp) '^)
           (^ (sexpr-value (1st-sub-exp exp)) (sexpr-value (2nd-sub-exp exp))))
       (else false)))
    (else false)))

(eq? (sexpr-value '13) 13)
(eq? (sexpr-value '(+ 1 3)) 4)
(eq? (sexpr-value '(+ 1 (^ 3 4))) 82)

(define (1st-sub-exp-2 aexp) (first aexp))

(define (operator-2 aexp) (first (rest aexp)))

(define (value-2 exp)
  (cond
    ((atom? exp) exp)
    ((list? exp)
     (cond
       ((eq? (operator-2 exp) '+)
           (+ (value (1st-sub-exp-2 exp)) (value (2nd-sub-exp exp))))
       ((eq? (operator-2 exp) '*)
           (* (value  (1st-sub-exp-2 exp)) (value (2nd-sub-exp exp))))
       ((eq? (operator-2 exp) '^)
           (^ (value  (1st-sub-exp-2 exp)) (value (2nd-sub-exp exp))))
       (else false)))
    (else false)))

(eq? (value-2 `(2 * 3)) 6)
(eq? (value-2 `13) 13)
(eq? (value-2 `(1 + 3)) 4)
(eq? (value-2 `(1 + (3 ^ 4))) 82)

(define (sero? n) (null? n))

(define (edd1 n) (cons '() n))

(define (zub1 n) (rest n))

(define (edd n m)
  (cond
    ((sero? m) n)
   (else (edd (edd1 n) (zub1 m)))))

(equal? (edd '(()) '(())) '(() ()))

(equal? (edd '(() ()) '(() ())) '(() () () ()))

(define (lat? x)
  (if (null? x) true
  (and (atom? (first x)) (lat? (rest x)))))

(eq? (lat? '((()) (() ()) (() () ()))) false)