(in-package #:parsonic)

(defparameter *emulate-stack-allocation-p* '(#-(or sbcl ccl) t))

(defun call-with-cons-pool/compile (thunk)
  (with-gensyms (cons-pool cons car cdr cons-alloc cons-free)
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
                (prog1 ,cons-pool (setf ,cons-pool ,cons))))
         (declare (ignorable #',cons-alloc #',cons-free) (inline ,cons-alloc ,cons-free))
         ,(labels ((codegen-cons (&optional (car nil carp) (cdr nil cdrp))
                     (if carp
                         (if cdrp
                             `(,cons-alloc ,car ,cdr)
                             `(,cons-free ,car))
                         #'codegen-cons))
                   (codegen-make-list (&optional (dimensions nil dimensionsp))
                     (if dimensionsp
                         (with-gensyms (list)
                           (when *emulate-stack-allocation-p*
                             (setf dimensions (loop :for size :in (ensure-list dimensions) :collect (- (abs size)))))
                           (push (cons list dimensions) *codegen-list-vars*)
                           list)
                         #'codegen-make-list)))
            (let ((*codegen-cons* #'codegen-cons)
                  (*codegen-make-list* #'codegen-make-list))
              (funcall thunk)))))))
