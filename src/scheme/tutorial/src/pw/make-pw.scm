;;; make password from the alist of probable spelling

(load "stat-spell.dat") ; *stat-spell* (alist for following characters) is in.

(define (alist->hash al mode)
  (let ((h (case mode
             ((eq) (make-eq-hash-table))
             ((eqv) (make-eqv-hash-table))
             ((equal) (make-equal-hash-table))
             ((string) (make-string-hash-table)))))
    (for-each (lambda (p)
                (hash-table/put! h (car p) (cdr p)))
              al)
    h))

(define *stat-spell-hash* (alist->hash *stat-spell* 'eqv))

(define (pw-random-select vec)
  (vector-ref vec (random (vector-length vec))))

(define (random00)
  (let loop ((i 0) (acc '()))
    (if (= i 2)
        (list->string acc)
	(loop (1+ i)
	      (cons (pw-random-select
		     '#(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
		    acc)))))

(define (occasional-upcase c)
  (if (< (random 10) 3)
      (char-upcase c)
      c))

(define (pw-enhance ls)
  (list->string
   (map (lambda (c)
          (cond
           ((char=? c #\Space) (pw-random-select  '#(#\- #\_ #\/  #\Space  #\. #\, #\@ #\? #\( #\))))
           ((char-alphabetic? c) (occasional-upcase c))
           (else c)))
        (cdr (reverse! ls)))))

(define (random-following alist)
  (let ((n (random (apply + (map cdr alist)))))
    (let loop ((j 0) (alist alist))
      (if (pair? alist)
	  (let* ((pair (car alist))
		 (k (+ j (cdr pair))))
	    (if (> k n)
		(car pair)
		(loop k (cdr alist))))))))

(define (make-pw h n)
  (let loop ((i 0) (c #\Space) (acc '()))
    (if (= i n)
        (string-append
         (pw-enhance (cons #\Space (cons c acc)))
         (random00))
	(loop (1+ i)
              (random-following (hash-table/get h c '((#\Space . 1))))
              (cons c acc)))))

(define (pw-candidates)
  (let loop ((i 0))
    (if (< i 10)
        (begin
          (display i)
          (display ": ")
          (write (make-pw *stat-spell-hash* (+ 9 (random 4))))
          (newline)
          (loop (1+ i)))
	'done)))
