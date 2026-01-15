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

(defmethod expand-expr :around ((op (eql 'rep)) &rest args)
  (destructuring-bind (parser &optional (from 0) (to most-positive-fixnum)) args
    (call-next-method op parser from to)))

(defparser opt (parser)
  (or parser (constantly nil)))

(defparser repsep (rep sep &optional (from 0) (to most-positive-fixnum))
  (or (cons rep (rep (progn sep rep) (max (1- from) 0) (1- to))) (rep (or) from 1)))

(defconstant eof +input-eof+)

(defparser eof ()
  (progn (eql eof) (constantly nil)))
