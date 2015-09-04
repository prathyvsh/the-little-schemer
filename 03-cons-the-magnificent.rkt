;; My definition
(define (rember a lat)
    (cond
    ((null? lat) '())
    ((eq? a (first lat)) (rest lat))
    (else (cons (first lat) (rember a (rest lat))))))

(define a 'mint)
(define lat '(lamb chops and mint jelly))
(rember a lat)
;; => '(lamb chops and jelly)

(define a 'mint)
(define lat '(lamb chops and mint flavored mint jelly))

(rember a lat)
;; => '(lamb chops and flavored mint jelly)

(define a 'toast)
(define lat '(bacon lettuce and tomato))

(rember a lat)
;; => '(bacon lettuce and tomato)

(define a 'cup)
(define lat '(coffee cup tea cup and hick cup))

(rember a lat)
;; => '(coffee tea cup and hick cup)

;; rember takes an atom and a lat as its arguments, and makes a new
;; lat with the first occurence of the atom in the old lat removed.

(define rember
  (lambda (a lat)
    (cond
     ((null? lat) '())
     (else (cond
            ((eq? (first lat) a) (rest lat))
            (else (rember a (rest lat))))))))

(define a 'bacon)

(define lat '(bacon lettuce and tomato))

(rember a lat)
;; => '(lettuce and tomato)

