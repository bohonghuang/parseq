(in-package #:parsonic)

(defmethod expand-expr ((op (eql 'eql)) &rest args)
  (destructuring-bind (object) args
    (expand `(satisfies (curry #'eql ,object)))))

(defmethod expand-quote ((vector vector))
  (expand
   `(progn
      ,@(map 'list (curry #'list 'eql) vector)
      (constantly ,vector))))

(defmethod expand-quote ((character character))
  (expand `(eql ,character)))
