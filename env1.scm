;In this implementation, the env is stored as a list of 2-element list
;The 2-element list element contain a key and a value to store the variable and value.
;extend-env will do a serch first if not find then just extend the environment with a new
;2-element list
;apply-env will simply do a search of all 2-eleent lists and if not found raise an error
(define (make-empty-env)
	()
)


(define (apply-env env v)
	(cond ( (null? env)
			(error "variable is not in this env"))
		  ( (equal? v (car (car env)))
			(second (car env))
			)
		  ( else
		  	(apply-env (cdr env) v)
		  )	
	)
)


(define (extend-env v val env)
	(cond ( (null? env) 
			(list (list v val)))
	      ( (equal? v (car (car env))) 
	      	(cons (list v val) (cdr env))
	      )
	      ( else
	      	(cons (car env) (extend-env v val (cdr env)))
	      )
	)
)


;for test only
;(define test-env
;	(extend-env 'a 1
;		(extend-env 'b 2
;			(extend-env 'c 3
;				(extend-env 'b 4
;					(make-empty-env)
;				)
;			)
;		)
;	)
;)
