(compile-file "stat-spell.scm")
(load "stat-spell")
;;; creating spelling data according to sicp_foreword.txt
(ss-make-dat "sicp_foreword.txt")

(compile-file "make-pw.scm")
(load "make-pw")

;;; making ten passwords using the spelling data.
(pw-candidates)
