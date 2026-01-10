(in-package #:parsonic)

(defun call-with-cons-pool/compile (thunk)
  (with-gensyms (cons-pool cons car cdr cons-alloc cons-free from to index)
    `(let ((,cons-pool nil))
       (declare (ignorable ,cons-pool))
       (flet ((,cons-alloc (,car ,cdr)
                (let ((,cons ,cons-pool))
                  (if ,cons
                      (progn
                        (setf ,cons-pool (cdr ,cons-pool)
                              (car ,cons) ,car
                              (cdr ,cons) ,cdr)
                        ,cons)
                      (cons ,car ,cdr))))
              (,cons-free (,cons &optional (,from 0) (,to most-positive-fixnum))
                (declare (type non-negative-fixnum ,from ,to))
                (when ,cons
                  (loop :for ,cons-free :on ,cons
                        :for ,index :of-type non-negative-fixnum :from 0
                        :when (= ,index ,from) :do (setf ,cons ,cons-free)
                        :until (= ,index (1- ,to))
                        :while (cdr ,cons-free)
                        :finally (return (setf (cdr ,cons-free) ,cons-pool ,cons-pool ,cons))))))
         (declare (ignorable #',cons-alloc #',cons-free) (inline ,cons-alloc ,cons-free))
         ,(funcall thunk cons-alloc cons-free)))))
