(in-package #:parseq)

(defgeneric expand-expr/compile (op &rest args)
  (:method (op &rest args)
    (apply #'expand-expr op args)))

(defvar *expand/compile-variables* nil)
(defvar *expand/compile-known* nil)

(defgeneric expand/compile (form)
  (:method ((expr list))
    (let ((*expand* #'expand/compile))
      (apply #'expand-expr/compile expr)))
  (:method ((symbol symbol))
    (assert (not (keywordp symbol)))
    (if-let ((cons (assoc symbol *expand/compile-variables*)))
      (expand/compile (cdr cons))
      symbol)))

(defun collect-parser-args (name args body)
  (declare (special collect-parser-args-recursive))
  (let ((parser-args nil))
    (flet ((collect-parser-args ()
             (let ((*expand* (lambda (object)
                               (typecase object
                                 (symbol (push object parser-args))
                                 (list
                                  (catch 'collect-parser-args
                                    (unless (member (car object) collect-parser-args-recursive)
                                      (apply #'expand-expr/compile object))))
                                 (t nil)))))
               (expand body)
               parser-args)))
      (if (boundp 'collect-parser-args-recursive)
          (let ((collect-parser-args-recursive (cons name collect-parser-args-recursive)))
            (declare (special collect-parser-args-recursive))
            (throw 'collect-parser-args
              (loop :for parser :in (collect-parser-args)
                    :do (expand (assoc-value args parser)))))
          (let ((collect-parser-args-recursive (list name)))
            (declare (special collect-parser-args-recursive))
            (collect-parser-args))))))

(defun %expand-expr/compile (name lambda-list args body)
  (let ((parser-args (collect-parser-args name args body)))
    (multiple-value-bind (parsers variables)
        (loop :for (variable . value) :in args
              :if (member variable parser-args)
                :collect (cons variable
                               (loop :for (target variables) := (list value *expand/compile-variables*)
                                       :then (loop :for ((name . value) . rest) :on variables
                                                   :when (eq name target)
                                                     :return (list value rest))
                                     :do (assert target)
                                     :unless (symbolp target) :return target))
                  :into parsers
              :else
                :collect (cons variable value) :into variables
              :finally (return (values parsers variables)))
      (if-let ((fdef (assoc (cons name parsers) *expand/compile-known* :test #'equal)))
        (let ((fname (etypecase (cdr fdef)
                       (boolean (setf (cdr fdef) (gensym (string name))))
                       (symbol (cdr fdef)))))
          (loop :for caller-fdef :in *expand/compile-known*
                :until (eq fdef caller-fdef)
                :unless (cdr caller-fdef)
                  :do (setf (cdr caller-fdef) t))
          `(parser/call ,fname . ,(mapcar #'cdr variables)))
        (let* ((lambda-list (remove-if (rcurry #'assoc parsers) lambda-list))
               (variables (mapcar (lambda (cons) (list (car cons) (cdr cons))) variables))
               (signature (list (cons name (mapcar #'cdr parsers)) lambda-list))
               (lambda-list (if (intersection lambda-list lambda-list-keywords)
                                (append lambda-list '(&initial) variables)
                                (progn
                                  (assert (equal lambda-list (mapcar #'car variables)))
                                  variables))))
          (let ((*expand/compile-variables* (append parsers *expand/compile-variables*))
                (*expand/compile-known* (cons (cons (cons name parsers) nil) *expand/compile-known*)))
            (let* ((result (expand/compile body))
                   (fname (cdr (first *expand/compile-known*)))
                   (result (if fname result `(parser/unit ,signature ,result))))
              (etypecase fname
                (boolean (if lambda-list `(parser/let nil ,lambda-list ,result) result))
                (symbol `(parser/let ,fname ,lambda-list ,result))))))))))
