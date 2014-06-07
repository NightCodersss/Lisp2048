(load "utility")

(define Server (lambda (host port id)
	(let  ((idle #T) (cfd (open-tcp-stream-socket host port)))
		(define is-idle (lambda ()
			idle
		))
		(define set-idle (lambda (v)
			(set! idle v)
		))
		(define send-command (lambda (command response)
			(begin
				(set-idle #F)
				(display command cfd)
				(newline cfd)
				(flush-output cfd)
				(display command)
				(newline)
				(create-thread #f
					(lambda ()
						(define loop (lambda ()
							(begin
								(sleep-current-thread 500)
								(response (eval (read (open-input-string (read-string (char-set #\linefeed) cfd))) system-global-environment))
							)
						))
						(loop)
						(set-idle #T)
					)
				)
			)
		))
		(define send-calculations-n-times (lambda (l n response)
			(send-command (list 'analyze-coefficients l n) response) 
		))

		(define get-name (lambda ()
			(string-append host ":" (number->string port 10))
		))

		(define dispatch (lambda (m)
			(cond
				((equal? m 'is-idle) is-idle)
				((equal? m 'send-command) send-command)
				((equal? m 'send-calculations-n-times) send-calculations-n-times)
				((equal? m 'get-name) get-name)
			)
		))
		dispatch
	)
))

(define Watcher (lambda (_serverlist)
	(let ((commands-queue '()) (serverlist _serverlist))
		(define get-result-pure (lambda (l n response)
			(set! commands-queue (push-to-queue commands-queue (list 'send-calculations-n-times l n response)))
		))

		(define get-idle-server (lambda ()
			(define get-idle (lambda (l)
				(cond
					((equal? l '()) 'no-server)
					(#T 
						(cond
							( (((car l) 'is-idle)) (car l) )
							(#T (get-idle (cdr l)))
						)
					)
				)
			))
			(get-idle serverlist)
		))

		(define get-result (lambda (l n response)
			(begin
			;	(message "get-result" l n)
				(cond
					((> n 1000) (begin
							(get-result-pure l 1000 response)
							(get-result l (- n 1000) response)
						    )
					)
					(#T (get-result-pure l n response))
				)
			)
		))

		(define get-task (lambda ()
			(let ((result (car commands-queue)))
				(begin
					(set! commands-queue (cdr commands-queue)) 
			;		(message "get-task result: " result "get-task commands-queue: " commands-queue)
					result
				)
			)
		))

		(define watcher-loop (lambda ()
			(begin
;				(apply message (cons "Watcher loop" (map (lambda (serv) ((serv 'is-idle))) serverlist)))
			;	(message "Command queue: " commands-queue)
				(sleep-current-thread 10)
				(cond
					((equal? commands-queue '()) '())
					(#T
						(let ((idle-server (get-idle-server)))
							(begin
							(cond 
								((equal? idle-server 'no-server) '())
					 			(#T 
									(let ((task (get-task)))
										(message "Task: " task)
										(apply (idle-server (car task)) (cdr task))
									)
								)
							)
							)
						)
					) 
				)
				(watcher-loop)
			)
		))

		(define start-loop (lambda ()
			(create-thread #f watcher-loop)
		))

		(define alldone (lambda ()
 			(and 
				(equal? commands-queue '()) 
				(reduce 
					#T 
					(lambda (serv acc) 
						(and 
							((serv 'is-idle)) 
							acc
						)
					)
					serverlist
				)
			)
		))

		(define dispatch (lambda (m)
			(cond
				((equal? m 'start-loop) start-loop)
				((equal? m 'get-result) get-result)
				((equal? m 'alldone) alldone)
			)
		))
		dispatch
	)
))

(define gen-server-list (lambda (host port n)
	(cond 
		((= n 0) '())
		(#T (cons (Server host port (random 1000000)) (gen-server-list host (+ port 1) (- n 1))))
	)
))
		
(define watch (Watcher (addList
			'() 
;		   	(gen-server-list "192.168.43.68" 1080 4)
			(gen-server-list "localhost" 1080 4)
		   ))
)
((watch 'start-loop))

(define pool0 (cons 0 0))
(define pool1 (cons 0 0))
(define pool2 (cons 0 0))
(define pool3 (cons 0 0))

(define push-to-queue (lambda (queue elem)
	(cond
		((equal? queue '()) (list elem))
		(#T (cons (car queue) (push-to-queue (cdr queue) elem)))
	)
))

(define get-full-result (lambda (k1 k2 k3 dk1 dk2 dk3 n)
	(let ((get-res (watch 'get-result)))
		(get-res
			(list 'list k1 k2 k3)
			n
			(lambda (x)
				(set! pool0 
					(cons 
						(+ 
							(* x n) 
							(car pool0)
						) 
						(+ 
							n 
							(cdr pool0)
						)
					)
				) 
			)
		)
		(get-res 
			(list 'list (+ k1 dk1) k2 k3)
			n
			(lambda (x)
				(set! pool1 
					(cons 
						(+ 
							(* x n) 
							(car pool1)
						) 
						(+ 
							n 
							(cdr pool1)
						)
					)
				) 
			)
		)
		(get-res
			(list 'list k1 (+ k2 dk2) k3)
			n
			(lambda (x)
				(set! pool2 
					(cons 
						(+ 
							(* x n) 
							(car pool2)
						) 
						(+ 
							n 
							(cdr pool2)
						)
					)
				) 
			)
		)
		(get-res 
			(list 'list k1 k2 (+ k3 dk3))
			n
			(lambda (x)
				(set! pool3 
					(cons 
						(+ 
							(* x n) 
							(car pool3)
						) 
						(+ 
							n 
							(cdr pool3)
						)
					)
				) 
			)
		)
	)
))
(define reset-pools (lambda ()
	(begin 
		(set! pool0 (cons 0 0))
		(set! pool1 (cons 0 0))
		(set! pool2 (cons 0 0))
		(set! pool3 (cons 0 0))
	)
))
(define gen-new-l (lambda ()
	(set! l (sum-of-lists l (multiplyVector step (pool-gradient))))
))
(define l (list 3. 1. 0.05))
(define dl (list 0.1 0.1 0.001))
(define step 0.1)
(define quantity-of-games 10000)


(define updatepool (lambda ()
	(begin 
		(cond 
			( ((watch 'alldone)) (begin 
				;	(message "alldone is " 
					(gen-new-l)
					(reset-pools)
					(get-full-result (car l) (cadr l) (caddr l) (car dl) (cadr dl) (caddr dl) quantity-of-games)
				))
			(#T (begin
				(sleep-current-thread 500)
				(display pool0)
				(newline)
				(display pool1)
				(newline)
				(display pool2)
				(newline)
				(display pool3)
				(newline)
				(display (pool-gradient))
				(newline)
				(display l)
				(newline)
			))
		)	
		(updatepool)
	)
))
(define gradient (lambda (f0 f1 f2 f3 d1 d2 d3)
	(normalizeVector (list 
		(/ (- f1 f0) d1)
		(/ (- f2 f0) d2)
		(/ (- f3 f0) d3)
	))
))
(define pool-gradient (lambda ()
	(gradient
		(car pool0)
		(car pool1)
		(car pool2)
		(car pool3)
		(car dl)
		(cadr dl)
		(caddr dl)
	)
))
(updatepool)
