;Function1
;Implement a function called (singleton? x) that returns #t if x is a list with exactly 1 element, and #f otherwise. For example:

;> (singleton? '(4 mouse ()))
;#f

;> (singleton? '(xy))
;#t

;> (singleton? 4)
;#f
(define singleton?
	(lambda (x)
		(and (list? x)
			 (not (null? x))
			 (null? (cdr x))
		)
	)
)

;Function2
;Implement a function called (my-make-list n x) that returns a list containing n copies of x. For example:

;> (my-make-list 3 'a)
;(a a a)

;> (my-make-list 2 '(1 2 3))
;((1 2 3) (1 2 3))

;> (my-make-list 2 (my-make-list 3 '(a b)))
;(((a b) (a b) (a b)) ((a b) (a b) (a b)))
(define my-make-list
	(lambda (n x)
		(cond ((<= n 0) 
				())
		      (else 
		      	(cons x (my-make-list (- n 1) x)))
		)
	)
)

;Function3
;Implement a function called (all-same? lst) that returns #t if lst is empty, or if all the elements in it are equal to each other (using equal?). For example:

;> (all-same? '())
;#t

;> (all-same? '(cat))
;#t

;> (all-same? '(cat cat cat))
;#t

;> (all-same? '(cat cat dog cat))
;#f
;You can assume lst is a valid list. If it’s not, it’s fine if your function crashes.
(define all-same?
	(lambda (lst)
		(cond ((null? lst)
				 #t)
			  ((singleton? lst)
			  	 #t)	
			  ((not (equal? (car lst) (car (cdr lst))))
			  	 #f)
			  (else 
			  	(all-same? (cdr lst)))
		)
	)
)

;Function4
;Implement a function called (my-iota n) that returns a list containing the numbers from 0 to n-1. For example:

;> (my-iota 0)
;()

;> (my-iota 1)
;(0)

;> (my-iota 2)
;(0 1)

;> (my-iota 5)
;(0 1 2 3 4)
;If n is 0 or less, then return the empty list.

;You can assume n is a valid integer. If it’s not, it’s fine if your function crashes.
(define my-iota
	(lambda (n)
		(cond ((<= n 0)
				())
			  (else
			  	(my-iota-helper n 0))
		)
	)
)

(define my-iota-helper
	(lambda (n x)
		(cond ((= n (+ x 1)) 
				(list x))
			  (else 
			  	(cons x (my-iota-helper n (+ x 1)))) 
		)
	)
)

;Function 5
;Implement a function called (my-length lst) that returns that returns the number of items in lst. For example:

;> (my-length '())
;0

;> (my-length '(a))
;1

;> (my-length '(a (b c)))
;2

;> (my-length '(a (b c) d))
;3
;You can assume lst is a valid list. If it’s not, it’s fine if your function crashes.
(define my-length
	(lambda (lst)
		(cond ((null? lst)
				 0)
		      (else 
		      	(+ 1 (my-length (cdr lst))))
		)
	)
)

;Function 6
;Implement a function called (nth lst i) that returns that returns the item at index location i in lst. The indexing is 0-based, so, the first element is at index location 0, the second element is at index location 1, and so on. For example:

;> (nth '(a b c) 0)
;a

;> (nth '(a b c) 1)
;b

;> (nth '(a b c) 2)
;c

;> (nth '(a b c) 3)
;;bad index
;You can assume lst is a valid list and i is a valid integer. If not, it’s fine if your function crashes.

;If i is less than 0, or if its greater than or equal to the length of lst, call the error function.
(define nth
	(lambda (lst i)
		(cond ((< i 0)
				(error "bad index"))
			  ((null? lst) 
			  	(error "bad index"))
			  ((= i 0) 
			  	(car lst))
			  (else 
			  	(nth (cdr lst) (- i 1)))
		)
	)
)

;Function 7
;Implement a function called (my-last lst) that returns the last element of lst. For example:

;> (my-last '(cat))
;cat

;> (my-last '(cat dog))
;dog

;> (my-last '(cat dog (1 2 3)))
;(1 2 3)

;> (my-last '())
;my-last: empty list
;Notice that calling my-last on the empty list prints the error message “my-last: empty list”. Use the error function to do this, e.g. (error "my-last: empty list").

;You can assume lst is a valid list. If it’s not, it’s fine if your function crashes.
(define my-last 
	(lambda (lst)
		(cond ((null? lst)
				(error "my-last: empty list"))
			  ((null? (cdr lst))
			   (car lst))
			  (else
			  	(my-last (cdr lst)))
		)
	)
)

;Function 8
;Implement a function called (middle lst) that returns a list that is the same as lst, but the first element and the last element have been removed. For example:

;> (middle '(a b c d e))
;(b c d)

;> (middle '(6 4 cat m egg))
;(4 cat m)
;If lst has 2, or fewer, elements, then return the empty list.

;You can assume lst is a valid list. If it’s not, it’s fine if your function crashes.
(define middle
	(lambda (lst)
		(cond ((<= (my-length lst) 2) 
				())
		      (else 
		      	(cdr (remove-last lst)))
		)
	)
)

(define remove-last
	(lambda (lst)
		(cond ((null? (cdr lst))
				())
			  (else
			  	(cons (car lst) (remove-last (cdr lst))))
		)
	)
)

;Function 9
;Implement a function called (my-filter pred lst) that returns a list containing just the elements of lst that satisfied the predicate function pred. For example:

;> (my-filter odd? '(5 7 0 -6 4))
;(5 7)

;> (my-filter odd? '(10 5 7 0 11 4))
;(5 7 11)

;> (my-filter list? '(hat (left right) 4 ()))
;((left right) ())

;> (my-filter (lambda (x) (or (= x 5) (< x 0))) '(5 6 9 -6 2 5 0 5))
;(5 -6 5 5)
;You can assume pred is a predicate function that takes one input, and returns either #t or #f. If it’s not, it’s fine if your function crashes.

;You can assume lst is a valid list. If it’s not, it’s fine if your function crashes.
(define my-filter
	(lambda (pred lst)
		(cond ((null? lst)
				())
			  ((pred (car lst))
			  	(cons (car lst) (my-filter pred (cdr lst))))
			  (else
			  	(my-filter pred (cdr lst)))
		)
	)
)

;Function 10
;Implement a function called (my-append A B) that returns a list that has all the elements of A followed by all the elements of B. For example:

;> (my-append '(1 2 3) '(4 5 6 7))
;(1 2 3 4 5 6 7)

;> (my-append '(1 2 3) '(4))
;(1 2 3 4)

;> (my-append '() '(4))
;(4)
;You can assume A and B are valid lists. If they’re not, it’s fine if your function crashes.
(define my-append 
	(lambda (A B)
		(cond ((null? A)
				 B)
			  (else
			  	(cons (car A) (my-append (cdr A) B)))	
		)
	)
)

;Function 11
;Implement a function called (append-all lol) that returns a list that has all the lists of lol appended into one list. For example:

;> (append-all '())
;()

;> (append-all '((a)))
;(a)

;> (append-all '((a) (b c)))
;(a b c)

;> (append-all '((a) (b c) (d)))
;(a b c d)

;> (append-all '((a) (b c) (d) (e f)))
;(a b c d e f)
;You can assume lol a valid list of lists, i.e. lol is a list whose elements are all lists. If lol is not a list of lists, it’s fine if your function crashes.
(define append-all
	(lambda (lol)
		(cond ((null? lol)
				())
			  (else
			  	(my-append (car lol) (append-all (cdr lol))))
		)
	)
)
;Function 12
;Implement a function called (my-sort lst) that returns the numbers on lst in sorted order. For example:

;> (my-sort '())
;()

;> (my-sort '(3))
;(3)

;> (my-sort '(4 1 3 7 5 5 1))
;(1 1 3 4 5 5 7)
;You can assume lst a valid list of numbers. If it’s not, it’s fine if your function crashes.

;It’s fine if your algorithm runs in quadratic time.

;Hint: Recursive sorting algorithms, like quicksort or mergesort, are good choices for Scheme
(define my-sort
	(lambda (lst)
		(cond ((null? lst) 
				())
			  (else
			  	(insert (car lst) (my-sort (cdr lst))))
		)
	)
)

(define insert
	(lambda (x lst)
		(cond ((null? lst)
				(list x))
			  ((< x (car lst))
			  	(cons x lst))
			  (else
			  	(cons (car lst) (insert x (cdr lst)))) 		
		)
	)
)

;Function 13 
;Implement a function called (all-bits n) that returns a list of 2n sub-lists, where each sub-list is a different pattern of n 0s and 1s. For example:

;> (all-bits 0)
;()

;> (all-bits 1)
;((0) (1))

;> (all-bits 2)
;((0 0) (0 1) (1 0) (1 1))
;The order of the sub-lists doesn’t matter, as long the returned list contains exactly all 2n possible bit lists.

;Important: your function should, at least in theory, work for any n no matter how big. Do not use any tricks that assume a limit on the size of n.

;If n is less than or equal to 0, return the empty list.

;You can assume n is a valid integer. If it’s not, it’s fine if your function crashes.
(define all-bits
	(lambda (n)
		(cond ((<= n 0)
				())
		 	  ((= n 1)
		 	  	'((0) (1)))
		 	  (else
		 	  	(all-bits-helper (all-bits (- n 1))))
		)
	)
)

(define all-bits-helper
	(lambda (lst)
		(cond ((null? lst)
				())
			  (else
			  	(my-append 
			  		(list (cons 0 (car lst)) (cons 1 (car lst)))
			  		(all-bits-helper (cdr lst))))
		)
	)
)

