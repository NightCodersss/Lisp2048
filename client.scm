
(define send-command-to-server (lambda (cfd command response)
	(begin 
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
			)
		)
	)
))

(define send-calculations-n-times (lambda (cfd l n response)
	(send-command-to-server cfd (list 'analyze-coefficients l n) response) 
))

(define get-result (lambda (serverlist l n response)
	(cond
		((equal? serverlist '()) '())
		(#T (cons (send-calculations-n-times (open-tcp-stream-socket (caar serverlist) (cdar serverlist)) l n response)))
	)
))

(send-command-to-server (open-tcp-stream-socket "localhost" 1082) '(+ 1 1) (lambda (x) (display (+ x 1))))
