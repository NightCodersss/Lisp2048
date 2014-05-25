(define print (lambda (f) 
	(cond 
		((equal? f '()) (newline))
		(#T (begin 
			(printline (car f))
			(newline)
			(print (cdr f))
		))
	)
))

(define printline (lambda (s)
	(cond 
		((equal? s '()) '())
		(#T (begin 
			(display (car s))
			(display " ")
			(printline (cdr s))
		))
	)
))
(define fill (lambda (l v n)
	(cond 
		((> n 0) 
			(cons 
			(cond 
				((equal? l '()) v)
				(#T (car l))
			)
			(fill (cond ((equal? l '()) '()) (#T (cdr l))) v (- n 1))))
		(#T l)
	)
))

(define reverse-columns (lambda (f)
	(cond
		((equal? f '()) '())
		(#T (cons (reverse (car f)) (reverse-columns (cdr f))))
	)
))
	
(define nth (lambda (l n)
	(cond
		((equal? l '()) '())
		((= n 0) (car l))
		(#T (nth (cdr l) (- n 1)))
	)
))
	
(define get-max (lambda (l)
	(cond
		((equal? l '()) -1)
		(#T (max (car l) (get-max (cdr l))))
	)
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
		
(define transpose (lambda (h)

	(define iter (lambda (h n)
		(cond
			((equal? h '()) '())
			(#T (cons (nth (car h) n) (iter (cdr h) n))) 
		)
	))

	(define iter2 (lambda (h n)
		(cond
			((> n (- (length h) 1)) '())
			(#T (cons (iter h n) (iter2 h (+ n 1))))
		)
	))

	(iter2 h 0)
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

(define Game2048 (lambda ()
	(let (
		(f '((0 0 0 0) (0 0 0 0) (0 0 0 0) (0 0 0 0)))
	)
		(define left (lambda (f)
			(define leftline (lambda (l) 
				(define merge-first (lambda (l)
					(cond 
						((= (length l) 1) l)
						((equal? (car l) (cadr l)) (cons (+ (car l) 1) (cddr l)))
						(#T l)
					)
				))
				(define merge (lambda (l n)
					(cond 
						((= n 0) (merge-first l))
						(#T (cons (car l) (merge (cdr l) (- n 1))))
					)
				))
				(define doit2 (lambda (l n)
					(cond 
						((> n (- (length l) 1)) l)
						(#T (doit2 (merge l n) (+ n 1)))
					)
				))
				(fill (doit2 (filter (lambda (e) (> e 0) ) l) 0) 0 4)
			))
			(define doit (lambda (f)
				(cond 
					((equal? f '()) '())
					(#T (cons (leftline (car f)) (doit (cdr f))))
				)
			))
			(doit f)
		))

		(define do-left (lambda ()
			(set! f (left f))
		))


		(define right (lambda (f)
			(reverse-columns (left (reverse-columns f)))
		))

		(define do-right (lambda ()
			(set! f (right f))
		))

		(define up (lambda (f)
			(transpose (left (transpose f)))
		))

		(define do-up (lambda ()
			(set! f (up f))
		))

		(define down (lambda (f)
			(transpose (right (transpose f)))
		))

		(define do-down (lambda ()
			(set! f (down f))
		))

		(define get-f (lambda () f))

		(define get-value (lambda (i j)
			(nth (nth f i) j)
		))
		
		(define set-value (lambda (i j v)
			(define set-item (lambda (l j v)
				(cond
					((equal? l '()) '())
					(#T (cons
						(cond
							((= j 0) v)
							(#T (car l))
						)
						(set-item (cdr l) (- j 1) v )
					))
				)
			))
			(define set-line (lambda (f i j v)
				(cond
					((equal? f '()) '())
					(#T (cons 
						(cond
							((= i 0) (set-item (car f) j v) )
							(#T (car f))
						)
						(set-line (cdr f) (- i 1) j v)
					))
				)
			))
			(set! f (set-line f i j v))
		))

		(define are-there-free-cells (lambda ()
			(> (length (filter 
				(lambda (l) 
					(> (length (filter 
						(lambda (x) 
							(= x 0)
						) 
						l)) 0)
				) 
				f))
				0
			)
		))

		(define new-random (lambda ()
			(cond 
				((are-there-free-cells)
					((lambda (pair)
						(cond 
							((> (get-value (car pair) (cdr pair)) 0) (new-random))
							(#T (set-value (car pair) (cdr pair) (+ (random 2) 1)))
						)
					) (cons (random 4) (random 4)))
				)
				(#T 'game-over)
			)
		))

		(define step (lambda (s)
			(cond
				((equal? s #\w) (do-up))
				((equal? s #\a) (do-left))
				((equal? s #\s) (do-down))
				((equal? s #\d) (do-right))
			)
		))

		(define set-f (lambda (ft)
			(set! f ft)
		))
		
		(define analyze (lambda ()
			(define free-quantity (lambda (f)
				(define free-in-line (lambda (l)
					(cond
						((equal? l '()) 0)
						(#T (+ (cond ((equal? (car l) 0) 1) (#T 0)) (free-in-line (cdr l)))) 
					)
				))

				(cond
					((equal? f '()) 0)
					(#T (+ (free-in-line (car f)) (free-quantity (cdr f))))
				)
			))
			(define quantity-of-mergeable-pairs (lambda (f)
				(define quantity-of-mergeable-pairs-in-line (lambda (l)
					(cond
						((<= (length l) 1) 0)
						(#T (+ (cond ((= (car l) (cadr l)) 1) (#T 0)) 
						       (quantity-of-mergeable-pairs-in-line (cdr l))
						    )
						)
					)
				))

				(define quantity-of-horizontal-mergeable-pairs (lambda (f)
					(cond
						((equal? f '()) 0)
						(#T (+ (quantity-of-mergeable-pairs-in-line (filter (lambda (x) (> x 0)) (car f))) 
						       (quantity-of-horizontal-mergeable-pairs (cdr f))
						    )
						)
					)
				))

				(+ (quantity-of-horizontal-mergeable-pairs f) (quantity-of-horizontal-mergeable-pairs (transpose f)))
			))
			
			(define quantity-of-chain-pairs (lambda (f)
				(define quantity-of-mergeable-pairs-in-line (lambda (l)
					(cond
						((<= (length l) 1) 0)
						(#T (+ (cond ((= (abs (- (car l) (cadr l))) 1) 1) (#T 0)) 
						       (quantity-of-mergeable-pairs-in-line (cdr l))
						    )
						)
					)
				))

				(define quantity-of-horizontal-mergeable-pairs (lambda (f)
					(cond
						((equal? f '()) 0)
						(#T (+ (quantity-of-mergeable-pairs-in-line (filter (lambda (x) (> x 0)) (car f))) 
						       (quantity-of-horizontal-mergeable-pairs (cdr f))
						    )
						)
					)
				))

				(+ (quantity-of-horizontal-mergeable-pairs f) (quantity-of-horizontal-mergeable-pairs (transpose f)))
			))

			(scalar 
				(list (free-quantity f) (quantity-of-mergeable-pairs f))
				(list 0.7 0.3)
			)
		))
			
		(define get-random-field (lambda (f)
			((lambda (a)
				(begin
					((a 'set-f) f)
					((a 'new-random))
					((a 'get-f))
				)
			) (Game2048) )
		))

		(define super-middle (lambda (l)
			(sqrt (reduce + 0 (map (lambda (x) (* x x)) l)))
		))
			
		(define get-analyze (lambda (ft n q)
			(cond 
				((= n 0) ((lambda (a)
					      (begin
						((a 'set-f) ft)
						((a 'analyze))
					      )
					) (Game2048))
				)
				(#T 
					(super-middle (list 
						(* q (get-analyze (left  (get-random-field ft)) (- n 1) (* q q)))
						(* q (get-analyze (right (get-random-field ft)) (- n 1) (* q q)))
						(* q (get-analyze (up    (get-random-field ft)) (- n 1) (* q q)))
						(* q (get-analyze (down  (get-random-field ft)) (- n 1) (* q q)))
					))
				)
			)	
		))

		(define get-optimal (lambda (f n q)
			
			((lambda (l)

			(display l)
			(newline)

			(nth (list #\a #\d #\w #\s) (index-of-max l))

			) (list 
				(get-analyze (left f) n q)
				(get-analyze (right f) n q)
				(get-analyze (up f) n q)
				(get-analyze (down f) n q)
			))
		))

		(define dispatch (lambda (m)
			(cond 
				((equal? m 'left) do-left)
				((equal? m 'get-f) get-f)
				((equal? m 'right) do-right)
				((equal? m 'up) do-up)
				((equal? m 'down) do-down)
				((equal? m 'step) step)
				((equal? m 'new-random) new-random)
				((equal? m 'analyze) analyze)
				((equal? m 'set-f) set-f)
				((equal? m 'get-optimal) (lambda (n q) (get-optimal f n q)))
			)
		))
		dispatch
	)
))

(define input (lambda ()
	((lambda (a)
		(cond
			((equal? a #\w) a)
			((equal? a #\a) a)
			((equal? a #\s) a)
			((equal? a #\d) a)
			(#T (input))
		)
	) (read-char))
))

(define g (Game2048))
(define turn (lambda ()
	(cond 
		((equal? ((g 'new-random)) 'game-over) (display "Game Over"))
		(#T (begin
			(print ((g 'get-f)))
			(display ((g 'analyze)))
			(newline)
			(display ((g 'get-optimal) 5 0.5))
			(newline)
			((g 'step) (input))
			(turn)
		))
	)
))

(define n-turns-analytically (lambda (n)
	(cond
		((<= n 0) (begin (display "done.") (n-turns-analytically (read))))
		((equal? ((g 'new-random)) 'game-over) (display "Game Over"))
		(#T 
			((lambda (opt)
			(begin
				(print ((g 'get-f)))
				(display ((g 'analyze)))
				(newline)
				(display opt)
				(newline)
				(newline)
				(newline)
				((g 'step) opt)
				(n-turns-analytically (- n 1))
			)) ((g 'get-optimal) 5 0.7))
		)
	)
))

(n-turns-analytically (read))
;(turn)