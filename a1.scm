
;Function1
(define singleton?
	(lambda (x)
		(and (list? x)
			 (not (null? x))
			 (null? (cdr x))
		)
	)
)

;Function2
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

