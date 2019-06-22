;In this implementation, the env is stored as a chain of closures
;each variable and value pair is store in one closures
;The make-empty-env will just return a lambda which contian an error message as base case
;whenever we extend the enviroment by calling extend-env, a new closure
;is created and contain the variable and value.
;the apply-env will call the closuer one by one and if they find the latest occurance 
;of the variable it will return the value or it will reach the base case and return an error


(define (make-empty-env)
	(lambda (var)
		(error "variable is not in this env")
	)
)


(define (apply-env env v)
	(env v)
)


(define (extend-env v val env)
	(lambda (var)
		(if (equal? v var)
			val
			(env var)
		)
	)
)


;for test only
;(define test-env
;	(extend-env 'a 10
;		(extend-env 'b 20
;			(extend-env 'c 30
;				(extend-env 'b 40
;					(make-empty-env)
;				)
;			)
;		)
;	)
;)