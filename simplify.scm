;simplify expression accourding the following rules:
;    (0 + e) simplifies to e
;    (e + 0) simplifies to e
;    (0 * e) simplifies to 0
;    (e * 0) simplifies to 0
;    (1 * e) simplifies to e
;    (e * 1) simplifies to e
;    (e / 1) simplifies to e
;    (e - 0) simplifies to e
;    (e - e) simplifies to 0
;    (e ** 0) simplifies to 1
;    (e ** 1) simplifies to e
;    (1 ** e) simplifies to 1
;    if n is a literal number, then (inc n) simplifies to the value of n + 1
;    if n is a literal number, then (dec n) simplifies to the value of n - 1

(define (nlist? n lst)
	(and (list? lst)
		 (= n (length lst))
	)
)

(define (is-uniary-expr? expr)
	(and (nlist? 2 expr)
		 (or (equal? 'inc (first expr))
		 	 (equal? 'dec (first expr)))
	)
)

(define (is-binary-expr? expr)
	(and (nlist? 3 expr)
		 (or (equal? '+ (second expr))
			 (equal? '- (second expr))
			 (equal? '* (second expr))
			 (equal? '/ (second expr))
			 (equal? '** (second expr)))
	)
)


(define (simplify expr)
	(cond ((or (number? expr) (symbol? expr))
			expr)
		  ((is-uniary-expr? expr)
		  	(let ((op (first expr))
		  		 (new_expr (simplify (second expr))))
		  		 (cond ((and (equal? 'inc op)
		  					 (number? new_expr))
		  			    (simplify (list new_expr '+ 1)))
		  			   ((and (equal? 'dec op)
		  	  			     (number? new_expr))
		  				(simplify (list new_expr '- 1))
		  			)
		  			   (else
		  			   	(list op new_expr)
		  			   )
		  		 )
		  	)
		  )
		  ((is-binary-expr? expr)
		  	(let ((op (second expr))
		  		 (new_expr_left (simplify (first expr)))
		  		 (new_expr_right (simplify (third expr))))
		  		 (cond 
		  		  ((and (equal? 0 new_expr_left) (equal? '+ op))
				    new_expr_right)
				  ((and (equal? 0 new_expr_right) (equal? '+ op))
				    new_expr_left)
				  ((and (equal? 0 new_expr_left) (equal? '* op))
				    0)
				  ((and (equal? 0 new_expr_right) (equal? '* op))
				    0)
				  ((and (equal? 1 new_expr_left) (equal? '* op))
				    new_expr_right)
				  ((and (equal? 1 new_expr_right) (equal? '* op))
				    new_expr_left)
				  ((and (equal? 1 new_expr_right) (equal? '/ op))
				    new_expr_left)
				  ((and (equal? 0 new_expr_right) (equal? '- op))
				    new_expr_left)
				  ((and (equal? new_expr_left new_expr_right) (equal? '- op))
				    0)
				  ((and (equal? 0 new_expr_right) (equal? '** op))
				    1)
				  ((and (equal? 1 new_expr_right) (equal? '** op))
				    (simplify (first expr)))
				  ((and (equal? 1 new_expr_left) (equal? '** op))
				    1)
				  (else 
				  	(list new_expr_left op new_expr_right)
				  )
		  		 )
		  	)
		  )
		  (else 
		   (error "Invalid expression!"))
	)
)

;for test only
;(define expr1 '((1 * (a + 0)) + 0))
;;Value: a

;(define expr2 '(((a + b) - (a + b)) * (1 * (1 + 0))))
;;Value: 0

;(define expr3 '((1 * a) + (b * 1)))
;;Value: (a + b)

;(define expr4 '((1 * a) + (b * 0)))
;;Value: a

;(define expr5 '(z ** (b * (dec 1))))
;;Value: 1