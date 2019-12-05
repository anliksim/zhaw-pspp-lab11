;; macro that prints an expression and it's evaluation
(defmacro pex (expr)
  (let ((tmp (gensym "tmp")))
    `(let ((,tmp ,expr))
       (format t "~S evaluates to ~S~%" ',expr ,tmp)
       ,tmp)))