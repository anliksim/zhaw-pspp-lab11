#!/usr/local/bin/sbcl --script

;; loads quicklisp
(load "~/quicklisp/setup.lisp")

;; loads json parser
(ql:quickload "cl-json")

;; loads common utils
(load "util.lisp")

;; loads commons functions
(load "base.lisp")

;;
;; Exercise
;;

(defvar *taskfile* "/usr/local/share/common-lisp/source/tasks.json")


;; Creates a stream of type string and decodes it assuming it is json.
;; The decoder creates a lisp structure with lisp tags (e.g. :ID) and strings, numbers, booleans, nil values.
;;
(pex (with-input-from-string
    (s (file-to-string *taskfile*))
  (json:decode-json s)))

(defun read-json (file-name)
  (with-input-from-string
      (s (file-to-string file-name))
    (json:decode-json s)))

(pex (read-json *taskfile*))


(defun getprop-fn (key values)
  (cdr (assoc key values)))

(pex (getprop-fn :result '((:RESULT . "SUCCESS") (:INTERFACE-VERSION . "1.0.3"))))

(setfun getprop (autocurry 'getprop-fn 2))
(pex (getprop :result '((:RESULT . "SUCCESS") (:INTERFACE-VERSION . "1.0.3"))))

(pex (getprop :tasks (read-json *taskfile*)))
(pex (getprop :tasks))
(defvar *tasks* (getprop :tasks (read-json *taskfile*)))


;; Filters all event from a sequence that where f evaluates to T i.e. removes them from the sequence
;;
(defun filter-fn (f seq)
  (remove-if-not f seq))

(setfun filter (autocurry 'filter-fn 2))

;; Same but if f evaluates to nil
;;
(defun reject-fn (f seq)
  (remove-if f seq))

(setfun reject (autocurry 'reject-fn 2))

;; Check if prop equals val
;;
(defun prop-eq (prop val)
  (pipeline (getprop prop) (partial #'equal val)))

(pex (filter (prop-eq :member "Scott") *tasks*))
(pex (filter (prop-eq :member "Scott")))


;; Remove all items that do not match the attribute
;;
(defun pick-fn (attrs obj)
  (remove-if-not #'(lambda (el) (member (car el) attrs)) obj))

(setfun pick (autocurry #'pick-fn 2))
(setfun forall (autocurry #'mapcar 2))

;; List with :due-date and :title
(pex (forall (pick '(:due-date :title)) *tasks*))


(defun date-to-universal (date)
  (multiple-value-bind (m d y)
      (values-list (mapcar 'parse-integer (string-split #\/ date)))
    (encode-universal-time 0 0 0 d m y)))

(pex (date-to-universal "11/15/2013"))
(pex (string-split #\/"11/15/2013"))


(defun sort-by-fn (f seq)
  (sort (copy-list seq)
        (lambda (a b) (< (funcall f a) (funcall f b)))))

(setfun sort-by (autocurry #'sort-by-fn 2))

(defun open-tasks (name)
  (pipeline
   (getprop :tasks)
   (filter (prop-eq :member name))
   (reject (prop-eq :complete t))
   (forall (pick '(:id :due-date :title :priority)))
   (sort-by (pipeline (getprop :due-date) #'date-to-universal))))


;; Tasks for Scott
(pex (funcall (open-tasks "Scott") *tasks*))
