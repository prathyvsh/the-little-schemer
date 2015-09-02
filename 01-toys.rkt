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





;; The Law of Car
;; The primitive car is defined only for non-empty lists. The cdr of
;; any non-empty list is always another list.
