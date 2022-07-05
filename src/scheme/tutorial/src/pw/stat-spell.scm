;;; make an alist of probable spelling from a given English text

(define (skip-char? c)
  (or (not (char-graphic? c))
      (memv c '(#\: #\; #\' #\" #\`))))

(define (ss-make-alist c alist)
  (let ((p (assv c alist)))
    (if p
        (begin
          (set-cdr! p (1+ (cdr p)))
          alist)
        (cons (cons c 1) alist))))

(define (ss-make-dat filename)
  (let ((char-hash (make-eqv-hash-table)))
    (with-input-from-file filename
      (lambda ()
	(let loop ((c #\Space))
	  (let ((c1 (read-char)))
            (if (not (eof-object? c1))
                (if (skip-char? c1)
                    (loop c)
                    (let ((c1 (char-downcase c1)))
		      (hash-table/put! char-hash c
				       (ss-make-alist c1 (hash-table/get char-hash c '())))
		      (loop c1))))))))
    (with-output-to-file "stat-spell.dat"
      (lambda ()
	(display "(define *stat-spell* \'(")
	(newline)
	(let loop ((alst (sort (hash-table->alist char-hash) 
			       (lambda (x y) (char<? (car x) (car y))))))
	  (if (pair? alst)
	      (begin
		(write (car alst))
		(newline)
		(loop (cdr alst)))))
        (display "))")
        (newline)))))
