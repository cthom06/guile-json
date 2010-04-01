(define-module (json reader))
(export json:read-string
	json:read-number
	json:read-bool
	json:read-null
	json:null
	json:read-list
	json:read-object
	json:read-value)
(use-modules (ice-9 rdelim))

; Unescapes \ escaped characters
(define (json:backslash fd)
	(let ((c (read-char fd)))
		(case c
			((#\n) "\n")
			((#\t) "\t")
			((#\f) "\f")
			((#\r) "\r")
			((#\b) "\b")
			((#\\) "\\")
			((#\") "\"")
			((#\/) "/")
			((#\u)
				(let ((num (string->number (string (read-char fd) (read-char fd) (read-char fd) (read-char fd)) 16)))
					(if (> num 255)
						(set! num 255)) ;FIXME
					(string (integer->char num)))))))
; This func is for representing JSON's null
(define (json:null) '())
(define (json:read-string fd)
	(let getstring ((current (read-delimited "\\\"" fd 'split)))
		(case (cdr current)
			((#\")
				(car current))
			((#\\)
				(let ((newc (json:backslash fd))  (temp (read-delimited "\\\"" fd 'split)))
					(list-set! temp 0 (string-append (car current) newc (car temp)))
					(getstring temp)))
			(else
				(error "Error parsing JSON")))))
; Helper function for getting the key-value pair from a map/object/whatever you wanna call it
(define (json:read-pair fd)
	(let ((key (json:read-string fd)))
		(if key
			(let t1 ((c (read-char fd)))
				(case c
					((#\newline #\ht #\space)
						(t1 (read-char fd)))
					((#\:)
						(let ((val (json:read-value fd)))
							(if val
								(list key val)
								(error "Error parsing JSON"))))
					(else (error "Error parsing JSON"))))
			(error "Error parsing JSON"))))
(define (json:read-object fd)
	(let t1 ((objhash (make-hash-table 3)))
		(case (read-char fd)
			((#\newline #\ht #\space)
				(t1 objhash))
			((#\")
				(let t2 ((pair (json:read-pair fd)))
					(if pair
						(begin
							(hash-set! objhash (car pair) (car (cdr pair)))
							(let t3 ((c (read-char fd)))
								(case c
									((#\newline #\ht #\space)
										(t3 (read-char fd)))
									((#\,)
										(t1 objhash))
									((#\})
										objhash)
									(else (error "Error parsing JSON")))))
						(error "Error parsing JSON"))))
			((#\})
				objhash))))
(define (json:read-list fd)
	(let t1 ((c (peek-char fd)) (r (list)))
		(case c
			((#\newline #\ht #\space)
				(read-char fd)
				(t1 (peek-char fd) r))
			((#\])
				(read-char fd)
				r)
			(else
				(set! r (append r (list (json:read-value fd))))
				(let t2 ((nc (read-char fd)))
					(case nc
						((#\newline #\ht #\space)
							(t2 (read-char fd)))
						((#\,)
							(t1 (peek-char fd) r))
						((#\])
							r)
						(else
							(error "Error parsing JSON"))))))))
(define (json:read-number fd)
	(let t1 ((c (read-char fd)) (b ""))
		(case c
			((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\+ #\- #\. #\e #\E)
				(t1 (read-char fd) (string-append b (string c))))
			(else
				(unread-char c fd)
				(string->number b)))))
(define (json:read-bool fd)
	(case (read-char fd)
		((#\t)
			(if (string=? "true" (string #\t (read-char fd) (read-char fd) (read-char fd)))
				#t))
		((#\f)
			(if (string=? "false" (string #\f (read-char fd) (read-char fd) (read-char fd) (read-char fd)))
				#f))))
(define (json:read-null fd)
	(if (string=? "null" (string (read-char fd) (read-char fd) (read-char fd) (read-char fd)))
		json:null
		(error "Error parsing JSON")))
(define (json:read-value fd)
	(let t1 ((c (read-char fd)))
		(case c
			((#\")
				(json:read-string fd))
			((#\{)
				(json:read-object fd))
			((#\[)
				(json:read-list fd))
			((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\-)
				(unread-char c fd)
				(json:read-number fd))
			((#\newline #\ht #\space)
				(t1 (read-char fd)))
			((#\t #\f)
				(unread-char c fd)
				(json:read-bool fd))
			((#\n)
				(unread-char c fd)
				(json:read-null fd))
			(else
				(error "Error parsing JSON")))))
