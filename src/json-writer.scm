(define-module (json writer))
(export json:dump) ; maybe we should export more than dump?
(use-modules (srfi srfi-1) (srfi srfi-13))
(define (json:dump-string s)
	(let ((ls (string->list s)))
		(list->string (append (fold (lambda (c p)
			(append p (case c
				((#\newline) '(#\\ #\n))
				((#\ht) '(#\\ #\t))
				((#\np) '(#\\ #\f))
				((#\cr) '(#\\ #\r))
				((#\bs) '(#\\ #\b))
				((#\") '(#\\ #\"))
				((#\\) '(#\\ #\\))
				((#\/) '(#\\ #\/))
				(else (list c)))))
		'(#\") ls) '(#\")))))

(define (json:dump-list l)
	(string-append
		"["
		(string-join (map json:dump l) ", ")
		"]"))

(define (json:dump-object o)
  (string-append "{"
		 (string-join
		  (hash-fold
		   (lambda (k v p)
		     (cons
		      (string-append (json:dump-string k) ": " (json:dump v))
		      p))
		   '()
		   o)
		  ", ")
		 "}")))

(define (json:dump-number n)
	(number->string n))
(define (json:dump-bool b)
	(if b
		"true"
		"false"))
(define (json:dump-null n)
	"null")
(define (json:dump obj)
	(cond
		((number? obj) (json:dump-number obj))
		((string? obj) (json:dump-string obj))
		((vector? obj) (json:dump-object obj)) ; Using vector? here because hash-table? wasn't working
		((list? obj) (json:dump-list obj))
		((boolean? obj) (json:dump-bool obj))
		(else (json:dump-null obj))))
