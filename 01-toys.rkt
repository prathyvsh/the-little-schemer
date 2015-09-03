#lang racket

;; Atom is a string of characters beginning with the letter a.

(or (symbol? 'atom) (symbol? (quote atom)))
;; => #t

(symbol? 'turkey)
;; => #t

(symbol? '1492)
;; => #t

(symbol? 'u)
;; => #t

(symbol? '*abc$) ; Special characters are allowed except for "(" and ")"
;; => #t

(or (list? '(atom)) (list (quote (atom))))
;; => #t

'(atom turkey or)
;; => #t

'(atom turkey) 'or ; is not a list but a collection of a list and a symbol.

(list? '((atom turkey) or))

;; All atoms and lists are S-expressions.

(list? '(how are you doing so far))
;; => #t

;; How many S-expressions are in the list?
(length '(how are you doing so far))
;; => 6
;; But not sure if I should include the whole expression
;; itself as another S-exp.

;; How many S-expressions are in the list?

(count '(((how) are) ((you) (doing so)) (far)))

;; => 3

(list '())
;; => #t
;; Special S-expression called null (or empty) list.

(symbol? '())
;; => #f
;; Lists are not symbols.

(list? '(() () () ()))
;; => #t

(first '(a b c))
;; => 'a

(first '((a b c) x y z))
;; => '(a b c)

;; (first 'hotdog)
;; You cannot use first on an atom.

;; (first '())
;; You cannot ask for the car of the empty list.

;; The Law of Car
;; The primitive car is defined only for non-empty lists.

(first '(((hotdogs)) (and) (pickle) (relish)))
;; => '((hotdogs))
;; List of lists.

(first (first '(((hotdogs)) (and))))
;; => '(hotdogs)

(rest '(a b c))
;; => '(b c)

(rest '((a b c) x y z))
;; => '(x y z)

(rest '(hamburger))
;; => '()

(rest '((x) t r))
;; => '(t r)

;; (rest 'hotdogs) and (rest '()) is invalid.

;; The Law of Cdr
;; The primitive car is defined only for non-empty lists. The cdr of
;; any non-empty list is always another list.

(first (rest '((b) (x y) ((c)))))
;; => '(x y)

(rest (rest '((b) (x y) ((c)))))
;; => '(((c)))

;; (rest (first '(a (b (c)) d))) is invalid.
;; Since (first '(a (b (c)) d)) is 'a and (rest 'a) is invalid.

;; first and rest always takes in non-empty lists as arguments.

(cons 'peanut '(butter and jelly))
;; => '(peanut butter and jelly)

(cons '(banana and) '(peanut butter and jelly))
;; => '((banana and) (peanut butter and jelly))

(cons '((help) this) '(is very ((hard) to learn)))
;; => '(((help) this is very (hard) to learn))

;; cons takes two arguments: first one is any S-exp and second is any list.

(cons '(a b (c)) '())         ; This one was a bit tricky since I coasted
;; => '((a b (c)))            ; along almost mindlessly so far.


(cons 'a '())
;; => '(a)

;; (cons '((a b c)) 'b) is invalid since 'b is not a list.

;; But (cons 'a 'b) works for any value.

(first (cons 'a 'b)) ; => 'a

(rest (cons 'a '(b))) ; => '(b)

;; (cons 'a 'b) is invalid since 'b is not a list.

;; The Law of Cons
;; The primitive cons takes two arguments.
;; The second argument to cons must be a list.
;; The result is a list.

(cons 'a (first '((b) c d)))
;; => (cons 'a '(b))
;; => '(a b)

(cons 'a (rest '((b) c d)))
;; => (cons 'a '(c d))
;; => '(a c d)

(and (null? '()) (null? (quote ())))
;; => #t

(null? '(a b c))
;; => #f

(null? 'spaghetti)
;; null? is only valid for lists but returns false for any presence of value.
;; I guess this is called nil punning.
;; => #f

;; The Law of Null?
;; The primitive null? is defined only for lists.

(define atom? (lambda (x) (and (not (pair? x)) (not (null? x)))))

(atom? 'harry)
;; => #t

(atom? '(Harry had a heap of apples))
;; => #f

;; atom? takes one argument and it can be any S-exp.

(atom? (first '(Harry had a heap of apples)))
;; => #t

(atom? (rest '(Harry had a heap of apples)))
;; => #f

(atom? (rest '(Harry)))
;; => #f since '() is not an atom

(atom? (first (rest '(swing low sweet cherry oat))))
;; => (atom? (first '(low sweet cherry oat)))
;; => (atom? 'low)
;; => #t

(atom? (first (rest '(swing (low sweet) cherry oat))))
;; => (atom? (first '((low sweet) cherry oat)))
;; => (atom? '(low sweet))
;; => #f

(eq? 'Harry 'Harry)
;; => #t

(eq? 'margarine 'butter)
;; => #f

;; eq? takes two arguments which are non-numeric atoms.

(eq? '() '(strawberry)) ; is invalid
;; But in practice lists may be arguments as well.
;; => #f

(eq? 6 7) ; is invalid as it needs non-numeric atoms.

(eq? (first '(Mary had a little lamb chop)) 'Mary)
;; => #t


(eq? (rest '(soured milk)) 'milk)
;; (eq? '(milk) 'milk)
;; => #f

(define l '(beans beans we need jelly beans))
(eq? (first l) (first (rest l)))
;; => #t
;; Because it compares the first and second atoms in the list.
