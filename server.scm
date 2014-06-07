(load "main")

(define clientloop (lambda (cfd env)
	(let 
		((instring (read-string (char-set #\linefeed) cfd)))
		(begin
			(display "from client: ")
			(display instring)
			(newline)
			(cond
				((eof-object? instring) '())
				((<= (string-length instring) 0) (clientloop cfd env))
				(#T 
					(begin 
						(display (eval (read (open-input-string instring)) env) cfd)
						(newline cfd)
						(flush-output cfd)
						(read-char cfd)
						(clientloop cfd env)
					)
				)
			)
		)
	)
))
(define www (lambda (serverport)
	(define sfd (open-tcp-server-socket serverport))
	(define serverloop (lambda ()
		(let 
			(
				(cfd (tcp-server-connection-accept sfd #t #f))
				(env (nearest-repl/environment))
			)
			(begin
				(clientloop cfd env)
				(close-port cfd)
				(serverloop)
			)
		)
	))
  	(serverloop)
))

(www (read))
