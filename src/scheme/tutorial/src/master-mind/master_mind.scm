;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; mastermind.scm
;;; by T.Shido
;;;
;;; User and computer try to locate the four-digit integer set by the opponents each other.
;;; One who locates the integer with fewer question is the winner.
;;; The four-digit integer contains four of numerals 0--9, like 0123, 3749 etc.
;;; The opponents should tell the guesser
;;; (1) number of numerals that are shared by the guessed and set numbers
;;; at wrong position (cows)
;;; and (2) number of numerals at collect position (bulls).
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The four-digit integers are represented by 10-cell vectors in the program
;;; The value of n-th cell is the number of column that n appears in the integer.
;;; in n is not appears the value is 0.
;;; for example, 1234 is represented as #(0 4 3 2 1 0 0 0 0 0) and
;;; 3916 as #(0 2 0 4 0 0 1 0 0 3).
;;; With this inner representation, the score of the guess can be calculated faster.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (1- x) (- x 1))

(define (char2int c)
  (- (char->integer c) (char->integer #\0)))

;;; converting a list of 4 numbers to the vector notation
(define (ls2nvec ls)
  (let ((vec (make-vector 10 0)))
    (let loop ((i (length ls)) (ls ls))
      (if (> i 0)
	  (begin
            (vector-set! vec (car ls) i)
            (loop (1- i) (cdr ls)))
          vec))))

(ls2nvec '(5 3 6 0)) ; => #(1 0 0 3 0 4 2 0 0 0)

;;; converting the vector notation to string
(define (nvec2int vec)
  (let loop ((i 0) (n 0))
    (if (= i 10)
        n
	(let ((j (vector-ref vec i)))
	  (loop (1+ i) (+ n (if (> j 0)
                                (* i (expt 10 (1- j)))
				0)))))))

(define (int2str i)
  (string-append
   (if (< i 1000) "0" "")
   (number->string i)))

;;; reading integer from stdin
(define (read-integer str)
  (string->number (read-from-stdin str)))

(define (read-from-stdin str)
  (display str)
  (newline)
  (read-line))

(define (write-to-stdout . ls)
  (for-each (lambda (obj) (display obj)) ls)
  (newline))

;;; convert numeral string to the vector representation.
(define (str2nvec str)
  (let ((vec (make-vector 10 0)))
    (let loop ((i (string-length str)) (ls (string->list str)))
      (if (pair? ls)
	  (begin
            (vector-set! vec (char2int (car ls)) i)
            (loop (1- i) (cdr ls)))
          vec))))

;;; calculating the score of guess
(define (scoring vec0 vec1)
  (let ((n (vector-length vec0)))
    (let loop ((i 0) (score 0))
      (if (< i n)
	  (let ((d0 (vector-ref vec0 i))
                (d1 (vector-ref vec1 i)))
            (loop (1+ i)
		  (+ score (if (and (< 0 d0) (< 0 d1))
                               (if (= d0 d1) 5 1)
                               0))))
          score))))

;;; show bulls and cows calculated from the score of user's guess
(define (show-user-score score)
  (write-to-stdout "Number of bulls and cows in your guess:" )
  (write-to-stdout "bulls: " (quotient score 5))
  (write-to-stdout "cows: " (modulo score 5))
  (newline))

;;; calculating the score of computer's guess from bulls and cows
(define (read-my-score gu0)
  (write-to-stdout "My guess is: " (int2str (nvec2int gu0)))
  (write-to-stdout "Give number of bulls and cows in my guess." )
  (let ((na5 (* 5 (read-integer "bulls: "))))
    (+ na5 (read-integer "cows: ")))) ; the score is calculated by (5 * bull + cow)

;;; reading the user guess
(define (read-user-guess)
  (newline)
  (str2nvec (read-from-stdin "Give your guess.")))

;;; shuffling the list of four-digit numbers
(define (shuffle-numbers ls0)
  (let ((vec (list->vector ls0)))
    (let loop ((n (vector-length vec)) (ls1 '()))
      (if (= n 0)
          ls1
	  (let* ((r (random n))
		 (v (vector-ref vec r)))
	    (vector-set! vec r (vector-ref vec (1- n)))
	    (loop (1- n) (cons v ls1)))))))

;;; making a list of four-digit numbers in which numeral 0--9 appear once
(define (make-numbers)
  (let ((ls1 '()))
    (letrec ((rec (lambda (i num ls)
		    (if (= i 4)
			(set! ls1 (cons (ls2nvec ls) ls1))
			(for-each 
			 (lambda (n)
			   (rec (1+ i) (delv n num) (cons n ls)))
			 num)))))
      (rec 0 '(0 1 2 3 4 5 6 7 8 9) '()))
    ls1))

(define (game-over sc0 sc1)
  (write-to-stdout
   (cond
    ((= sc0 sc1) "Draw")
    ((> sc0 sc1) "I won.")
    (else "You won.")))
  'game-over)

(define (scoring-user-guess an0 gu1)
  (let ((sc1 (scoring an0 gu1)))
    (show-user-score sc1)
    sc1))

;;; Practical main function. tail recursive.
(define (mastermind-rec an0 candidates)
  (if (null? candidates)
      (error "Error. You gave wrong score for my guess, probably.")
      (let ((gu0 (car candidates)))
	(let ((sc1 (scoring-user-guess an0 (read-user-guess)))
	      (sc0 (read-my-score gu0)))
	  (if (or (= sc0 20) (= sc1 20))
	      (game-over sc0 sc1)
	      (mastermind-rec an0 
			      (keep-matching-items 
			       (cdr candidates)
			       (lambda (x) (= (scoring gu0 x) sc0)))))))))

;;; The main function called from the top-level
(define (mastermind)
  (let ((ls0 (make-numbers)))
    (mastermind-rec (list-ref ls0 (random (length ls0))) (shuffle-numbers ls0))))

(compile-file "mastermind.scm")
(load "mastermind")
(mastermind)
