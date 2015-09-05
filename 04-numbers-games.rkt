(define atom? (lambda (x) (and (not (pair? x)) (not (null? x)))))

(atom? 14)
;; => #t

(number? -3)
;; => #t

(number? 3.14159)
;; => #t

(and (number? -3) (number? 3.14159))
;; => #t

(add1 67)
;; => 68

(sub1 5)
;; => 4

(sub1 0)
;; => -1 but we only consider nonnegative numbers in this chapter.

(zero? 0)
;; => #t

(zero? 1492)
;; => #f

(+ 46 12)
;; => 58

;; My definition
(define (+ n m)
  (cond
   ((zero? m) n)
   (else (add1 (+ n (sub1 m))))))

(+ 3 4)
;; => 7

zero? is equivalent of null? for numbers.
add1 is equivalent of cons for numbers.

(- 14 3)
;; => 11

(- 17 9)
;; => 8

(- 18 25)
;; => -7 but no answer for our purposes.

(define (- n m)
  (cond
  ((zero? m) n)
  (else (sub1 (- n (sub1 m))))))


;; One thing I notice here is that the last case in this expression
;; can be written as (- (sub1 n) (sub1 m))
;; This I think is probably similar to the accumulator approach where
;; the stack is not built up but immediately calculated during one step.

(- 4 3)
;; => 1

;; My definition
(define (tup? l)
  (cond
  ((null? l) #t)
  (else (and (number? (first l)) (tup? (rest l))))))

(tup? '(2 11 3 79 47 6))
;; => #t

(tup? '(8 55 5 555))
;; => #t

(tup? '(1 2 8 apple 4 3))
;; => #f

(tup? '(3 (7 4) 13 9))
;; => #f

(tup? '())
;; => #t

;; My definition
(define (addtup t)
  (cond
  ((null? t) 0)
  (else (+ (first t) (addtup (rest t))))))

(addtup '(3 5 2 8))
;; => 18

(addtup '(15 6 7 12 3))
;; => 43

;; Natural terminal condition for numbers
;; (zero? n)

;; Natural recursion on a number
;; (sub1 n)

(* 5 3)
;; => 15

(* 13 4)
;; => 52

(define (* n m)
  (cond
  ((zero? m) 0)
  (else (+ n (* n (sub1 m))))))

(* 4 3)

;; (* 12 3) = (+ 12 (* 12 2))
;;          = (+ 12 (+ 12 (* 12 1)))
;;          = (+ 12 (+ 12 (+ 12 (* 12 0))))
;;          = (+ 12 12 12 0)

;; Zero is the terminal condition for * because
;; it will no affect the combinator +. That is
;; n + 0 = n.

;; Thinking in this manner consing a null on to
;; a list doesn't change it's value?

(define (tup+ t1 t2)
  (cond
   ((or (null? t1)
        (null? t2)) '())
   (else (cons (+ (first t1) (first t2))
               (tup+ (rest t1) (rest t2))))))

(tup+ '(3 6 9 11 4) '(8 5 2 0 7))
;; => (11 11 11 11 11)

(tup+ '(2 3) '(4 6))
;; => '(6 9)

(tup+ '(3 7) '(4 6))
;; => '(7 13)

;; My definition : Modified to retain the rest of the longer tuple.
(define (tup+ t1 t2)
  (cond
   ((and (null? t1) (null? t2)) '())
   ((null? t1) t2)
   ((null? t2) t1)
   (else (cons (+ (first t1)
                  (first t2))
               (tup+ (rest t1)
                     (rest t2))))))

(tup+ '(3 7) '(4 6 8 1))
;; => '(7 13 8 1)

(tup+ '(3 7 8 1) '(4 6))
;; =. '(7 13 8 1)

;; Refined
(define (tup+ t1 t2)
  (cond
;;   ((and (null? t1) (null? t2)) '()) ; Because it will be checked by one of the two following lines.
   ((null? t1) t2)
   ((null? t2) t1)
   (else (cons (+ (first t1)
                  (first t2))
               (tup+ (rest t1)
                     (rest t2))))))

(> 12 133)
;; => #f

(> 120 11)
;; => #t

;; My definition
(define (> a b)
  (cond
   ((zero? a) #f)
   ((zero? b) #t)
   (else (> (sub1 a)
            (sub1 b)))))

(> 0 0)
;; => #f

;; Classic.
;; Does the order of the two previous answers matter?
;; Yes. Think first, then try.
;; Don't try and then think.

;; The order matters because if you define
;; (zero? b) first, it means that (> 0 0)  passes
;; where as it should return false, so (zero? a)
;; should be put first.

;; My definition
(define (< n m)
  (cond
   ((zero? m) #f)
   ((zero? n) #t)
   (else (< (sub1 n) (sub1 m)))))

(< 0 0)
;; => #f

(< 4 6)
;; => #t

(< 8 3)
;; => #f

(< 6 6)
;; => #f

;; My definition
(define (= n m)
  (cond
   ((and (zero? m) (zero? n)) #t)
   ((zero? m) #f)
   ((zero? n) #f)
   (else (= (sub1 m) (sub1 n)))))

(= 4 3)
;; => #f

(= 9 9)
;; => #t

(= 9 3)
;; => #f

;; Rewrite
(define (= n m)
  (cond
   ((or (> n m) (< n m)) #f)
   (else #t)))

(= 4 3)
;; => #f

(= 9 9)
;; => #t

(= 9 3)
;; => #f

;; My definition
(define (^ n m)
  (cond
   ((zero? m) 1)
   (else (* n (^ n (sub1 m))))))

(^ 1 1)
;; => 1

(^ 2 3)
;; => 16

(^ 5 3)
;; => 125

(define (quotient n m)
    (cond
     ((< n m) 0)
     (else (add1 (quotient (- n m) m)))))

(quotient 4 1)
;; => 4

(quotient 27 3)
;; => 9

(quotient 10 9)
;; => 1

(quotient 15 4)
;; => 3

(define (length l)
  (cond
   ((null? l) 0)
   (else (add1 (length (rest l))))))

(length '(hotdogs with mustard sauerkraut and pickles))
;; => 6

(length '(ham and cheese on rye))
;; => 5

 (define (pick n lat)
   (cond
    ((= n 1) (first lat))
    (else (pick (sub1 n) (rest lat)))))

(pick 4 '(lasagna spahghetti ravioli macaroni meatball))
;; => macaroni

(pick 0 '(a))
;; Infinite loop because the termination condition is at 1

