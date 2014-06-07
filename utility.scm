(define nth (lambda (l n)
	(cond
		((equal? l '()) '())
		((= n 0) (car l))
		(#T (nth (cdr l) (- n 1)))
	)
))

(define message (lambda args
	(define display-message (lambda (args)
		(cond
			((equal? args '()) 'unspecified) 
			(#T 
				(begin
					(display (car args))
					(newline)
					(display-message (cdr args))
				)
			)
		)
	))
	(display-message args)
))

	
(define get-max (lambda (l)
	(apply max l)
))

(define get-index (lambda (l v n)
	(cond
		((equal? l '()) -1)
		((= v (car l)) n)
		(#T (get-index (cdr l) v (+ n 1)))
	)
))

(define index-of-max (lambda (l)
	(get-index l (get-max l) 0)
))
		
(define reduce (lambda (f acc l)
	(cond
		((equal? l '()) acc)
		(#T (reduce f (f acc (car l)) (cdr l)))
	)
))

(define scalar (lambda (l1 l2)
	(cond
		((equal? l1 '()) 0)
		((equal? l2 '()) 0)
		(#T (+ (* (car l1) (car l2)) (scalar (cdr l1) (cdr l2))))
	)
))
(define addLists (lambda (ls)
	(reduce addList '() ls)
))
(define addList (lambda (l1 l2)
	(cond 
		((equal? l1 '()) l2)
		(#T (cons (car l1) (addList (cdr l1) l2)))
	)
))

(define count-n (lambda (l n)
	(reduce (lambda (x y) (+ x (cond ((= y n) 1) (#T 0)))) 0 l)
))

(define list-k-n (lambda (k n)
	(cond 
		((> n 0) (cons k (list-k-n k (- n 1))))
		(#T '())
	)
))

(define vectorLength (lambda (l)
	(sqrt (reduce + 0 (map (lambda (x) (* x x)) l)))
))

(define normalizeVector (lambda (v)
	(cond 
		((= (vectorLength v) 0) (map (lambda (x) 0) v))
		(#T (map (lambda (x) (/ x (vectorLength v))) v))
	)
))

(define multiplyVector (lambda (c v)
	(map (lambda (x) (* x c)) v)
))

(define sum-of-lists (lambda (l1 l2)
	(cond
		((equal? l1 '()) '())
		((equal? l2 '()) '())
		(#T (cons (+ (car l1) (car l2)) (sum-of-lists (cdr l1) (cdr l2))))
	)
))

(define set-1 (lambda (k n)
	(cond
		((= n 0) '())
		((= k 0) (cons 1 (set-1 -1 (- n 1))))
		(#T (cons 0 (set-1 (- k 1) (- n 1)))) 
	)
))

(define n-lambda (lambda (l n)
	(cond
		((= n 0) '())
		(#T (cons (l) (n-lambda l (- n 1))))
	)
))

(define average (lambda (l)
	(/ (reduce + 0 l) (length l))
))

(define union addList)

