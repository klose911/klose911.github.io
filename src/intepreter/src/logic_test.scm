;;;;;;;;;;;;;;;;;;;;;;;
;; Logic Programming ;;
;;;;;;;;;;;;;;;;;;;;;;;

(define (initialize-data-base rules-and-assertions)
  (define (deal-out r-and-a rules assertions)
    (cond ((null? r-and-a)
           (set! THE-ASSERTIONS (list->stream assertions))
           (set! THE-RULES (list->stream rules))
           'done)
          (else
           (let ((s (query-syntax-process (car r-and-a))))
             (cond ((rule? s)
                    (store-rule-in-index s)
                    (deal-out (cdr r-and-a)
                              (cons s rules)
                              assertions))
                   (else
                    (store-assertion-in-index s)
                    (deal-out (cdr r-and-a)
                              rules
                              (cons s assertions))))))))
  (let ((operation-table (make-table)))
    (set! get (operation-table 'lookup-proc))
    (set! put (operation-table 'insert-proc!)))
  (put 'and 'qeval conjoin)
  (put 'or 'qeval disjoin)
  (put 'not 'qeval negate)
  (put 'lisp-value 'qeval lisp-value)
  (put 'always-true 'qeval always-true)
  (deal-out rules-and-assertions '() '()))

;; Do following to reinit the data base from microshaft-data-base
;;  in Scheme (not in the query driver loop)
;; (initialize-data-base microshaft-data-base)

(define microshaft-data-base
  '(
    (address (Bitdiddle Ben) (Slumerville (Ridge Road) 10))
    (job (Bitdiddle Ben) (computer wizard))
    (salary (Bitdiddle Ben) 60000)

    (address (Hacker Alyssa P) (Cambridge (Mass Ave) 78))
    (job (Hacker Alyssa P) (computer programmer))
    (salary (Hacker Alyssa P) 40000)
    (supervisor (Hacker Alyssa P) (Bitdiddle Ben))

    (address (Fect Cy D) (Cambridge (Ames Street) 3))
    (job (Fect Cy D) (computer programmer))
    (salary (Fect Cy D) 35000)
    (supervisor (Fect Cy D) (Bitdiddle Ben))

    (address (Tweakit Lem E) (Boston (Bay State Road) 22))
    (job (Tweakit Lem E) (computer technician))
    (salary (Tweakit Lem E) 25000)
    (supervisor (Tweakit Lem E) (Bitdiddle Ben))

    (address (Reasoner Louis) (Slumerville (Pine Tree Road) 80))
    (job (Reasoner Louis) (computer programmer trainee))
    (salary (Reasoner Louis) 30000)
    (supervisor (Reasoner Louis) (Hacker Alyssa P))

    (supervisor (Bitdiddle Ben) (Warbucks Oliver))

    (address (Warbucks Oliver) (Swellesley (Top Heap Road)))
    (job (Warbucks Oliver) (administration big wheel))
    (salary (Warbucks Oliver) 150000)

    (address (Scrooge Eben) (Weston (Shady Lane) 10))
    (job (Scrooge Eben) (accounting chief accountant))
    (salary (Scrooge Eben) 75000)
    (supervisor (Scrooge Eben) (Warbucks Oliver))

    (address (Cratchet Robert) (Allston (N Harvard Street) 16))
    (job (Cratchet Robert) (accounting scrivener))
    (salary (Cratchet Robert) 18000)
    (supervisor (Cratchet Robert) (Scrooge Eben))

    (address (Aull DeWitt) (Slumerville (Onion Square) 5))
    (job (Aull DeWitt) (administration secretary))
    (salary (Aull DeWitt) 25000)
    (supervisor (Aull DeWitt) (Warbucks Oliver))

    (can-do-job (computer wizard) (computer programmer))
    (can-do-job (computer wizard) (computer technician))

    (can-do-job (computer programmer)
		(computer programmer trainee))

    (can-do-job (administration secretary)
		(administration big wheel))

    (rule (lives-near ?person-1 ?person-2)
	  (and (address ?person-1 (?town . ?rest-1))
               (address ?person-2 (?town . ?rest-2))
               (not (same ?person-1 ?person-2))))

    (rule (same ?x ?x))

    (rule (wheel ?person)
	  (and (supervisor ?middle-manager ?person)
               (supervisor ?x ?middle-manager)))

    (rule (outranked-by ?staff-person ?boss)
	  (or (supervisor ?staff-person ?boss)
              (and (supervisor ?staff-person ?middle-manager)
		   (outranked-by ?middle-manager ?boss))))
    ))


;;; Query 
(and (job ?person (computer programmer))
     (address ?person ?where))

;; (and (job (fect cy d) (computer programmer)) (address (fect cy d) (cambridge (ames street) 3)))
;; (and (job (hacker alyssa p) (computer programmer)) (address (hacker alyssa p) (cambridge (mass ave) 78)))

(or (supervisor ?x (Bitdiddle Ben))
    (supervisor ?x (Hacker Alyssa P)))

;; (or (supervisor (tweakit lem e) (bitdiddle ben)) (supervisor (tweakit lem e) (hacker alyssa p)))
;; (or (supervisor (reasoner louis) (bitdiddle ben)) (supervisor (reasoner louis) (hacker alyssa p)))
;; (or (supervisor (fect cy d) (bitdiddle ben)) (supervisor (fect cy d) (hacker alyssa p)))
;; (or (supervisor (hacker alyssa p) (bitdiddle ben)) (supervisor (hacker alyssa p) (hacker alyssa p)))

(and (supervisor ?x (Bitdiddle Ben))
     (not (job ?x (computer programmer))))

;; (and (supervisor (tweakit lem e) (bitdiddle ben)) (not (job (tweakit lem e) (computer programmer))))

(and (salary ?person ?amount)
     (lisp-value > ?amount 30000))

;; (and (salary (scrooge eben) 75000) (lisp-value > 75000 30000))
;; (and (salary (warbucks oliver) 150000) (lisp-value > 150000 30000))
;; (and (salary (fect cy d) 35000) (lisp-value > 35000 30000))
;; (and (salary (hacker alyssa p) 40000) (lisp-value > 40000 30000))
;; (and (salary (bitdiddle ben) 60000) (lisp-value > 60000 30000))

(assert! (rule (append-to-form () ?y ?y))) 

(assert! (rule (append-to-form (?u . ?v) ?y (?u . ?z))
	       (append-to-form ?v ?y ?z))) 

;;; Query input:
(append-to-form (a b) (c d) ?z)
;;; Query results:
(append-to-form (a b) (c d) (a b c d))

;;; Query input:
(append-to-form (a b) ?y (a b c d))
;;; Query results:
 (append-to-form (a b) (c d) (a b c d))

;;; Query input:
(append-to-form ?x ?y (a b c d))
;;; Query results:
(append-to-form (a b c d) () (a b c d))
(append-to-form () (a b c d) (a b c d))
(append-to-form (a) (b c d) (a b c d))
(append-to-form (a b) (c d) (a b c d))
(append-to-form (a b c) (d) (a b c d))

;;; Query input
(append-to-form (a b) (c e) (a b c d))
;;; Query Output
