
(define (atom? x) (or (symbol? x) (number? x)))

;; lat : list -> boolean
;; Checks if a list is constructed out of atoms
(define (lat? x)
  (if (null? x) true
  (and (atom? (first x)) (lat? (rest x)))))

(lat? '(Jack Sprat could eat no chicken fat))
;; => #t

(lat? '((Jack) Sprat could eat no chicken fat))
;; => #f

(lat? '(Jack (Sprat could) eat no chicken fat))
;; => #f

(lat? '())
;; => #t

;; A lat is a list of atoms

(define lat?
  (lambda (l)
    (cond
     ((null? l) #t)
     ((atom? (car l)) (lat? (cdr l)))
     (else #f))))

;; There is a clever thing done here that instead
;; of using and which was my first impulse, the
;; authors devised a continuous check based on
;; control flow rather than accumulating
;; as I did with the case of and.

;; Doubts:
;; Is it more performant?

(lat? '(bacon and eggs))
;; => #t

(lat? '(bacon (and eggs)))
;; => #f

(define l1 '())

(define l2 '(d e f g))

(or (null? l1) (null? l2))
;; => #t

(define l1 '(a b c))

(define l2 '(atom))

(or (null? l1) (null? l2))
;; => #f


(or ...) asks two questions, one at a time. If the first one is true it stops and answers true. Otherwise it asks the second question and answers with whatever the second question answers.

(define a 'tea)

(define lat '(coffee tea or milk))

(define member?
  (lambda (a lat)
  (cond
  ((null? lat) #f)
  (else (or (eq? a (first lat))
            (member? a (rest lat)))))))

(member? a lat)
;; => #t

(define a 'poached)

(define lat '(fried eggs and scrambled eggs))

(member? a lat)
;; => #f


(define a 'meat)

(define lat '(mashed potatoes and meat gravy))

(member? a lat)
;; => #t

;; else is a question whose value is always true.

(define a 'liver)

(define lat '(bagels and lox))

(member? a lat)
;; => #f

;; Summary
;; Like the way the concepts were introduced. Was a bit too thorough for someone who has had prior experience.
