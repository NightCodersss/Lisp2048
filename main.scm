 (declare (usual-integrations))

(define (parallel-execute . procs)
  (map thread-wait
         (map (lambda (proc) (thread proc))
	             procs)))

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
(define get-value (lambda (f i j)
	(nth (nth f i) j)
))

(define Game2048 (lambda (cl)
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
							((> (get-value f (car pair) (cdr pair)) 0) (new-random))
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
			;	(reduce + 0 (map (lambda (x) (* (- x 1) (- x 1))) (get-chains f)))
				(get-max (get-chains f))
			))

			(scalar 
				(list (free-quantity f) (quantity-of-mergeable-pairs f) (quantity-of-chain-pairs f))
				cl
			)
		))
			
		(define get-random-field (lambda (f)
			((lambda (a)
				(begin
					((a 'set-f) f)
					((a 'new-random))
					((a 'get-f))
				)
			) (Game2048 cl) )
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
					) (Game2048 cl))
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

;			(display l)			(newline)

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

(define g (Game2048 (list 3.0 0.4 0.0005)))
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
(define addLists (lambda (ls)
	(reduce addList '() ls)
))
(define addList (lambda (l1 l2)
	(cond 
		((equal? l1 '()) l2)
		(#T (cons (car l1) (addList (cdr l1) l2)))
	)
))

(define without-ij (lambda (f i j)
	(define without-ij-line (lambda (l j)
		(cond
			((equal? l '()) '())
			((= j 0) (cons (- 2) (without-ij-line (cdr l) (- j 1))))
			(#T (cons (car l) (without-ij-line (cdr l) (- j 1))))
		)
	))
	(cond
		((equal? f '()) '())
		((= i 0) (cons (without-ij-line (car f) j) (without-ij (cdr f) (- i 1) j)))
		(#T (cons (car f) (without-ij (cdr f) (- i 1) j)))
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

(define get-chains (lambda (f)
	(define dfs (lambda (f x y)
		(define get-cell (lambda (x y v xp yp)
			(cond 
				((or (< x 0) (< y 0) (> x 3) (> y 3)) 0)
				((not (= (abs(- (get-value f x y) v )) 1)) 1)
				(#T (dfs (without-ij f xp yp) x y))
			)
		))
		(+ 
			(get-cell (- x 1) (- y 0) (get-value f x y) x y)
			(get-cell (+ x 1) (- y 0) (get-value f x y) x y)
			(get-cell (- x 0) (- y 1) (get-value f x y) x y)
			(get-cell (- x 0) (+ y 1) (get-value f x y) x y)
		)
	))
	(define loop2 (lambda (i j)
		(cond 
			((>= j 0)
				(addLists (list
					(list (dfs f i j))
					(loop2 i (- j 1))
				))
			)
			(#T '())
		) 
	))

	(define loop1 (lambda (i)
		(cond 
			((>= i 0)
				(addLists (list
					(loop2 i 3)
					(loop1 (- i 1)) 
				))
			)
			(#T '())
		)
	))
	(define vipilivalka (lambda (l n)
		(begin 
;			(display l)
			(cond
				((> n 0) (addList (list-k-n n (/ (count-n l n) n)) (vipilivalka l (- n 1))))
				(#T '())
			)
		)
	))
	(filter (lambda (x) (not (= x 1))) (vipilivalka (loop1 3) 16))
))

(define n-turns-analytically (lambda (g n acc)
	(cond
		((<= n 0) (begin (display "n-turns-analytically done.") acc))
		((equal? ((g 'new-random)) 'game-over) (begin (display "Game Over") (print ((g 'get-f))) acc))
		(#T 
			((lambda (opt)
			(begin
;				(print ((g 'get-f)))
;				(display ((g 'analyze)))
;				(newline)
;				(display opt)
;				(newline)
;				(newline)
;				(newline)
				((g 'step) opt)
				(n-turns-analytically g (- n 1) (+ acc ((g 'analyze))))
			)) ((g 'get-optimal) 1 1.0))
		)
	)
))

;(n-turns-analytically g (read))

(define average (lambda (l)
	(/ (reduce + 0 l) (length l))
))

(define n-lambda (lambda (l n)
	(cond
		((= n 0) '())
		(#T (cons (l) (n-lambda l (- n 1))))
	)
))

(define analyze-coefficients (lambda (l n)
	((lambda (ll)
		(* 1.0 (begin (display ll) (newline) (average ll)))
	) (n-lambda (lambda () (play-to-game-over (Game2048 l))) n))
))


(define set-1 (lambda (k n)
	(cond
		((= n 0) '())
		((= k 0) (cons 1 (set-1 -1 (- n 1))))
		(#T (cons 0 (set-1 (- k 1) (- n 1)))) 
	)
))

(define vectorLength (lambda (l)
	(sqrt (reduce + 0 (map (lambda (x) (* x x)) l)))
))

(define normalizeVector (lambda (v)
	(map (lambda (x) (/ x (vectorLength v))) v)
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

(define grad (lambda (l dl dx)
	(define nth-set (lambda (n)
		(sum-of-lists l (multiplyVector (scalar dl (set-1 n (length l))) (set-1 n (length l))))
	))
	(define derivative (lambda (n gl0)
		(/ 
			(- (analyze-coefficients (nth-set n) 30) gl0) 
			(scalar 
				dl 
				(set-1 n (length dl))
			)
		)
	))
	(define gradVector (lambda (i gl0)
		(cond
			((= i (length l)) '())
			(#T (cons (derivative i gl0) (gradVector (+ i 1) gl0)))
		)
	))
	((lambda (analyze)
			(normalizeVector (sum-of-lists l (multiplyVector dx (normalizeVector analyze))))
	) (gradVector 0 (analyze-coefficients l 30)) ) 
))

(define grad-n-times (lambda (n l dl dx)
	(begin
		(display "l is: ")
		(display l)
		(newline)
		(display (analyze-coefficients l 30))
		(newline)
		(cond
			((<= n 0) l)
			(#T (grad-n-times (- n 1) (grad (normalizeVector l) dl dx) dl dx))
		)
	)
))

(define play-to-game-over (lambda (g)
	(cond
		((equal? ((g 'new-random)) 'game-over) 0)
		(#T 
			(begin
				((g 'step) ((g 'get-optimal) 1 1.0))
				(+ 1 (play-to-game-over g))
			)
		)
	)
))

;(grad-n-times 10 (list 1.0 0.1 0.001) (list 0.1 0.1 0.0001) 0.01)

;(define gg (Game2048 (grad-n-times 100 (list 3.0 1.0 0.0005) (list 0.1 0.1 0.0001) 0.01)))

;(display (analyze-coefficients (list 3.0 0.7 0.0005)))
;(newline)
;(display (analyze-coefficients (list 3.0 0.7 0.0005)))
;(newline)
;(display (analyze-coefficients (list 3.0 0.7 0.0005)))
;(newline)
;(display (analyze-coefficients (list 3.0 0.7 0.0005)))
;(newline)
;(display (analyze-coefficients (list 3.0 0.7 0.0005)))
;(newline)

;(n-turns-analytically gg 1000 0)

;((lambda (l)
;	((lambda (gr)
;		(begin
;			(display gr)
;			(display (sum-of-lists gr (map (lambda (x) (* x (- 1))) l)))
;		)
;	) (grad (normalizeVector l) (list 0.1 0.1 0.0001) 0.01))
;) (list 1.0 0.1 0.001))
;(display (average (list (n-turns-analytically g 100 0) (n-turns-analytically g 100 0) (n-turns-analytically g 100 0))))


;(turn)
