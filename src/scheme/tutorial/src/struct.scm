(define-structure book title authors publisher year isbn)

(define bazaar 
  (make-book 
   "The Cathedral and the Bazaar"
   "Eric S. Raymond"
   "O'Reilly"
   1999
   0596001088)) ; bazaar

(define-structure (book keyword-constructor copier) 
  title authors publisher year isbn)

(define bazaar 
  (make-book 
   'title "The Cathedral and the Bazaar"
   'authors "Eric S. Raymond"
   'publisher "O'Reilly"
   'year 1999    
   'isbn 0596001088))

(book? bazaar) ; #t 

(define cathedral (copy-book bazaar)) ; cathedral
(book-title bazaar) ; "The Cathedral and the Bazaar"

(book-year bazaar) ; 1999   
(set-book-year! bazaar 2001)
(book-year bazaar) ; 2001
