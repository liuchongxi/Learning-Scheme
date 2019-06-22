(load "env1.scm")

(define (nlist? n lst)
	(and (list? lst)
		 (= n (length lst))
	)
)

(define (is-uniary-op? expr op)
	(and (nlist? 2 expr)
		 (equal? op (first expr))
	)
)

(define (is-inc? expr) (is-uniary-op? expr 'inc))
(define (is-dec? expr) (is-uniary-op? expr 'dec))

(define (is-binary-op? expr op)
	(and (nlist? 3 expr)
		 (equal? op (second expr))
	)
)

(define (is-add? expr) (is-binary-op? expr '+))
(define (is-sub? expr) (is-binary-op? expr '-))
(define (is-mul? expr) (is-binary-op? expr '*))
(define (is-div? expr) (is-binary-op? expr '/))
(define (is-exp? expr) (is-binary-op? expr '**))


(define (myeval expr env)
	(cond ((number? expr)
			expr)
		  ((symbol? expr)
		  	(apply-env env expr))
		  ((is-add? expr)
		  	(+ (myeval (first expr) env)
		  	   (myeval (third expr) env))
		   )
		  ((is-sub? expr)
		  	(- (myeval (first expr) env)
		  	   (myeval (third expr) env))
		   )
		  ((is-mul? expr)
		  	(* (myeval (first expr) env)
		  	   (myeval (third expr) env))
		   )
		  ((is-div? expr)
		  	(if (= 0 (myeval (third expr) env))
		  		(error "divide 0 error")
		  		(/ (myeval (first expr) env)
		  	       (myeval (third expr) env))
		  	)
		   )
		  ((is-exp? expr)
		  	(expt (myeval (first expr) env)
		  	      (myeval (third expr) env))
		   )
		  ((is-inc? expr)
		  	(+ 1 (myeval (second expr) env))
		  )
		  ((is-dec? expr)
		  	(- 1 (myeval (second expr) env))
		  )
		  (else
		  	(error "Invalid expression"))
	)
)


; for test only
;(define env1
;    (extend-env 'x -1
;        (extend-env 'y 4
;            (extend-env 'x 1
;                (make-empty-env))))
;)

;(define env2
;    (extend-env 'm -1
;        (extend-env 'a 4
;            (make-empty-env)))
;)

;(define env3
;    (extend-env 'q -1
;        (extend-env 'r 4
;            (make-empty-env)))
;)