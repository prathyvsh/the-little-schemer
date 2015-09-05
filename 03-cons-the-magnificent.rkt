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

(define a 'and)

(define lat '(bacon lettuce and tomato))

(rember a lat)

(define rember
  (lambda (a lat)
    (cond
     ((null? lat) '())
     (else (cond
            ((eq? (first lat) a) (rest lat))
            (else (cons (first lat) (rember a (rest lat)))))))))

(define a 'and)

(define lat '(bacon lettuce and tomato))

(rember a lat)
;; => '(bacon lettuce tomato)

;; I am getting a feeling of unwinding and winding up the stack
;; as I move through this finger exercises.

;; Simplify only when you have corrected the function.
;; Make the function's structure same as the argument's structure.
;; Then simplify.

(define a 'sauce)

(define lat '(soy sauce and tomato sauce))

(rember a lat)
;; => '(soy and tomato sauce)

;; My definition
(define (get-first l)
  (cond
   ((null? l) '())
   (else (first l))))

(define (firsts ls)
  (cond
  ((null? ls) '())
  (else (cons (get-first (first ls)) (firsts (rest ls))))))

(firsts '((apple peach pumpkin)
          (plum pear cherry)
          (grape raisin pea)
          (bean carrot eggplant)))
;; => '(apple plum grape bean)

(define l '((a b) (c d) (e f)))

(firsts l)
;; => '(a c e)

(firsts '())
;; => '()

(define l '((five plums) (four) (eleven green oranges)))
(firsts l)
;; => '(five four eleven)

(define l '(((five plums) four)
            (eleven green oranges)
            ((no) more)))

(firsts l)
;; => '((five plums) eleven (no))

;; My definition

(define (get-second l)
  (first (rest l)))

(define (seconds ls)
  (cond
   ((null? ls) '())
   (else (cons (get-second (first ls)) (seconds (rest ls))))))

(define l '((a b) (c d) (e f)))

(seconds l)
;; => '(b d f)

;; There is an identification of the recursion patern detailed.
;; (else (cons
;; (car (car l)) ; typical element
;; (firsts (cdr l)) ; natural recursion


;; My definition
(define (insertR new old lat)
  (cond
   ((null? lat) '())
   ((eq? (first lat) old) (cons (first lat) (cons new (rest lat))))
   (else (cons (first lat) (insertR new old (rest lat))))))

(insertR 'jalapeno 'and '(tacos tamales and salsa))
;; => '(tacos tamales and jalapeno salsa)

(define new 'e)

(define old 'd)

(define lat '(a b c d f g d h))

(insertR new old lat)

;; => '(a b c d e f g d h)

;; InsertR inserts new to the right of old, if old is present in the lat.


(define new 'topping)

(define old 'fudge)

(define lat '(ice cream with fudge for dessert))

(insertR new old lat)
;; => '(ice cream with fudge topping for dessert)

;; My attempt
(define (insertL new old lat)
  (cond
   ((null? lat) '())
   ((eq? (first lat) old) (cons new lat))
   (else (cons (first lat) (insertL new old (rest lat))))))

(insertL new old lat)
;; => '(ice cream with topping fudge for dessert)

(define (subst new old lat)
  (cond
   ((null? lat) '())
   ((eq? (first lat) old) (cons new (rest lat)))
   (else (cons (first lat) (subst new old (rest lat))))))

(subst new old lat)
;; => '(ice cream with topping for dessert)

(define (subst2 new o1 o2 lat)
  (cond
   ((null? lat) '())
   ((or (eq? (first lat) o1)
       (eq? (first lat) o2)) (cons new (rest lat)))
   (else (cons (first lat) (subst2 new o1 o2 (rest lat))))))

(define new 'vanilla)

(define o1 'chocolate)

(define o2 'banana)

(define lat '(banana ice cream with chocolate topping))

(subst2 new o1 o2 lat)
;; => '(vanilla ice cream with chocolate topping)

(define (multirember a lat)
  (cond
   ((null? lat) '())
   ((eq? a (first lat)) (multirember a (rest lat)))
   (else (cons (first lat) (multirember a (rest lat))))))

(define a 'cup)

(define lat '(coffee cup tea cup and hick cup))

(multirember a lat)
;; => '(coffee tea and hick)
  
(define (multiinsertR new old lat)
  (cond
   ((null? lat) '())
   ((eq? (first lat) old) (cons (first lat) (cons new (multiinsertR new old (rest lat)))))
   (else (cons (first lat) (multiinsertR new old (rest lat))))))

(define new 'and)

(define old 'cup)

(define lat '(coffee cup tea cup and hick cup))

(multiinsertR new old lat)

;; => '(coffee cup and tea cup and and hick cup and)

(define (multiinsertL new old lat)
  (cond
   ((null? lat) '())
   ((eq? (first lat) old) (cons new (cons old (multiinsertL new old (rest lat)))))
   (else (cons (first lat) (multiinsertL new old (rest lat))))))

(define new 'fried)

(define old 'fish)

(define lat '(chips and fish or fish and fried))

(multiinsertL new old lat)

;; => '(chips and fried fish or fried fish and fried)
