;;;;;;;;;;;;;;;;;;;;;;;
;; Input from a file ;;
;;;;;;;;;;;;;;;;;;;;;;;

(define (read-file file-name)
  (let ((p (open-input-file file-name)))
    (let loop((ls1 '()) (c (read-char p)))
      (if (eof-object? c)
	  (begin
	    (close-input-port p)
	    (list->string (reverse ls1)))
	  (loop (cons c ls1) (read-char p))))))

(read-file "hello.txt")
;; "Hello world!\r\nScheme is an elegant programming language.\r\n"

(display (read-file "hello.txt")) 
;;  Hello world!
;; Scheme is an elegant programming language.

;;; call-with-input-file
(define (read-file file-name)
  (call-with-input-file file-name
    (lambda (p)
      (let loop((ls1 '()) (c (read-char p)))
	(if (eof-object? c)
	    (begin
	      (close-input-port p)
	      (list->string (reverse ls1)))
	    (loop (cons c ls1) (read-char p)))))))

(display (read-file "hello.txt")) 
;;  Hello world!
;; Scheme is an elegant programming language.

;;; with-input-file
(define (read-file file-name)
  (with-input-from-file file-name
    (lambda ()
      (let loop((ls1 '()) (c (read-char)))
	(if (eof-object? c)
	    (list->string (reverse ls1))
	    (loop (cons c ls1) (read-char)))))))

(display (read-file "hello.txt")) 
;;  Hello world!
;; Scheme is an elegant programming language.

;;;;;;;;;;;;;;;;;;;;;;;;
;; Read S expressions ;;
;;;;;;;;;;;;;;;;;;;;;;;;
(define (s-read file-name)
  (with-input-from-file file-name
    (lambda ()
      (let loop ((ls1 '()) (s (read)))
	(if (eof-object? s)
	    (reverse ls1)
	    (loop (cons s ls1) (read)))))))

(s-read "paren.txt")
					; =>  ('(Hello world! Scheme is an elegant programming language.)
;;  '(Lisp is a programming language ready to evolve.))
