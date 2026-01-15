(in-package #:parseq)

(defmethod expand-expr :around ((op (eql 'satisfies)) &rest args)
  (destructuring-bind (predicate) args
    (if (equal predicate '(constantly nil))
        (call-next-method)
        (call-next-method
         op (with-gensyms (result)
              `(lambda (,result)
                 (unless (eql ,result +input-eof+)
                   (funcall ,predicate ,result))))))))

(defmethod expand-expr :around ((op (eql 'or)) &rest args)
  (case (length args)
    (0 (expand '(satisfies (constantly nil))))
    (1 (expand (first args)))
    (t (apply #'call-next-method (reduce (curry #'list 'or) args :from-end t)))))

(defmethod expand-expr :around ((op (eql 'rep)) &rest args)
  (destructuring-bind (parser &optional (from 0) (to most-positive-fixnum)) args
    (call-next-method op parser from to)))
