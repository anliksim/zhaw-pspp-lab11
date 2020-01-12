#!/usr/local/bin/sbcl --script

(load "util.lisp")


(pex (+ (* 2 2 2) (- 6 5)))
; 9

(pex (cons 9 (cdr '(1 2 3))))
; (9 2 3)

;(pex (let ((m '(m))) (append s s)))
; unbound variable

(pex (car (mapcar #'(lambda (n) (list 3 n)) '(4 1 9 23))))
; (3 4)

(defun matches (seq1 seq2)
  (cond ((or (null seq1) (null seq2)) t)
	((or (null (car seq1))
	     (null (car seq2)))
	 (matches (cdr seq1) (cdr seq2)))
	((eql (car seq1) (car seq2))
	 (matches (cdr seq1) (cdr seq2)))
	(t (eql (car seq1) (car seq2)))))

(pex (matches nil '(1 2 3)))
(pex (matches '(1 2 3) '(1 2 3)))
(pex (matches '(1 2 4) '(1 2 3)))
(pex (matches '(1 nil 3) '(1 2 3 )))
(pex (matches '(1 2) '(1 2 3)))


;; returns list of 5 days to mimic test setup
(defun next-n-days (days)
  (list
   '((0) 26 2 2012)
   '((1) 27 2 2012)
   '((2) 28 2 2012)
   '((3) 29 2 2012)
   '((4) 1 3 2012)
  ))

(pex (next-n-days 5))

(defvar *events* '(
		   ((28 2) "Geburtstag Peter")
		   ((12 6) "Geburtstag Maria")
		   ((24 12) "Weihnachten")
		   ((28) "Miete überweisen")
		   ((1 3 2012) "Zahnarzt")
		   )
  )

(pex *events*)


(defun events-on-date (date events)
  (remove-if-not #'(lambda (event) (matches (cdr date) (car event)))
	  events))

(pex (events-on-date '((2) 28 2 2012) *events*))


(defun events-next-n-days (days events)
  (apply #'append
	 (mapcar #'(lambda (date)
		     (mapcar #'(lambda (event)
				 (list (caar date) (cadr event)))
			     (events-on-date date events)))
		 (next-n-days days))))


(pex (events-next-n-days 5 *events*))




