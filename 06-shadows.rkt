#lang racket

(eq? (quote a) 'a)
;; => #t

(eq? 'a 'a)
;; => #t

(define atom? (lambda (x) (and (not (pair? x)) (not (null? x)))))

(define (op? m)
  (cond
   ((null? m) #t)
   ((or (eq? (first m) '+)
        (eq? (first m) '*)
        (eq? (first m) '^)) (numbered? (rest m)))
   (else #f)))

;; My definition
(define (numbered? m)
  (cond
   ((null? m) #t)
   ((atom? m) (number? m))
   ((atom? (first m)) (cond
                       ((number? (first m)) (op? (rest m)))
                       (else #f)))
   ((list? (first m)) (and (numbered? (first m))
                           (numbered? (rest m))))
   (else #f)))


(numbered? 1)
;; => #t

(numbered? '(3 + (4 ^ 5)))
;; => #t

(numbered? '(2 * sausage))
;; => #f

;; A simplified version is provided as:
(define (numbered? aexp)
  (cond
   ((atom? aexp) (number? aexp))
   (else (and (numbered? (first aexp))
              (numbered? (first (rest aexp)))))))

;; This function doesn't work on some corner cases like:
;; '() and some other cases but is operationally the same
;; as above version and more elegant.


