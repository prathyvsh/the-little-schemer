
(define atom? (lambda (x) (and (not (pair? x)) (not (null? x)))))

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
;; Is it more beautiful than my definition?
;; Is it more performant?

(lat? '(bacon and eggs))
;; => #t

(lat? '(bacon (and eggs)))
;; => #f
