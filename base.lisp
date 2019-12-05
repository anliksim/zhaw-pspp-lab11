(defun file-to-string (path)
  (with-open-file (stream path)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      data)))

(defun file-to-string (path)
  (with-open-file (stream path)
    (apply #'concatenate
           (cons 'string
                 (loop
                   for line = (read-line stream nil 'eof)
                   until (eq line 'eof)
                   collect (format nil "~A~%" line))))))

(defun string-split (c strg &optional (start 0))
  (let ((end (position c strg :start start)))
    (cond (end (cons (subseq strg start end)
                     (string-split c strg (+ end 1))))
          (t (list (subseq strg start))))))


(defun autocurry (f numargs)
  (lambda (&rest args)
    (cond ((>= (length args) numargs)
           (apply f args))
          ((>= (length args) 1)
           (lambda (&rest restargs) (apply f (append args restargs))))
          (t (error "Function must be called with at least one argument")))))

(defmacro setfun (symb fun)
  `(prog1 ',symb (setf (symbol-function ',symb) ,fun)))

(defun pipeline (&rest funcs)
  (lambda (&rest args)
    (car (reduce
          (lambda (ar f) (list (apply f ar)))
          funcs
          :initial-value args))))

(defun partial (f &rest args)
  (lambda (&rest more-args)
    (apply f (append args more-args))))
