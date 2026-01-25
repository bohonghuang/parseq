(in-package #:parsonic)

(defparameter *emulate-stack-allocation-p* '(#-(or sbcl ccl) t))

(defun call-with-cons-pool/compile (thunk)
  (with-gensyms (cons-pool cons car cdr list cons-next cons-alloc cons-free from to cons-free-macro copy-list)
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
              (,cons-free (,cons)
                (loop :for ,cons-free :on ,cons
                      :unless (cdr ,cons-free)
                        :return (setf (cdr ,cons-free) ,cons-pool ,cons-pool ,cons)))
              (,copy-list (,list)
                (loop :for (,car . ,cdr) :on (or ,list (return))
                      :for ,cons := (or ,cons-pool (return (copy-list ,list))) :then ,cons-next
                      :for ,cons-next := (cdr ,cons)
                      :do (setf (car ,cons) ,car)
                      :while ,cons-next
                      :finally
                         (setf (cdr ,cons) (copy-list ,cdr))
                         (return (shiftf ,cons-pool ,cons-next)))))
         (declare (ignorable #',cons-alloc #',cons-free #',copy-list) (inline ,cons-alloc ,cons-free))
         (macrolet ((,cons-free-macro (,cons &optional ,from ,to)
                      (if ,from
                          (progn
                            (check-type ,from (eql 0))
                            (check-type ,to positive-fixnum)
                            (once-only (,cons)
                              `(setf (cdr (nthcdr ,(1- ,to) ,,cons)) ,',cons-pool ,',cons-pool ,,cons)))
                          `(,',cons-free ,,cons))))
           ,(let ((*codegen-cons* (lambda (car &optional (cdr nil cdrp))
                                    (if cdrp
                                        `(,cons-alloc ,car ,cdr)
                                        (if (and (listp car) (eq (first car) 'subseq))
                                            `(,cons-free-macro (shiftf ,(second car) nil) ,(third car) ,(fourth car))
                                            `(,cons-free-macro (shiftf ,car nil))))))
                  (*codegen-make-list* (lambda (size)
                                         (with-gensyms (list)
                                           (when *emulate-stack-allocation-p*
                                             (setf size (- size)))
                                           (push (cons list size) *codegen-list-vars*)
                                           list)))
                  (*codegen-copy-list* (lambda (list) `(,copy-list ,list))))
              (funcall thunk)))))))
