(in-package :cl-user)
(defpackage :coolgate/cpu/arm64
  (:use :cl))
(in-package :coolgate/cpu/arm64)

(defun assemble-forms (forms)
  (print forms)
  (with-output-to-string (asm)
    (loop for form in forms do (assemble-form form asm))))

(defun assemble-form (form stream)
  (if (symbolp form)
      (format stream "~a:~%" form)
      (assemble-line form stream)))

(defun assemble-line (line stream)
  (let ((line (alexandria:ensure-list line)))
    (format stream "~a ~{~a~^, ~}~%" (car line)
            (mapcar (lambda (value) (assemble-value value))
                    (cdr line)))))

(defun walk-tree (tree test new)
  (labels ((s (subtree)
             (cond ((funcall test subtree) (funcall new subtree))
                   ((atom subtree) subtree)
                   (t (let ((car (s (car subtree)))
                            (cdr (s (cdr subtree))))
                        (if (and (eq car (car subtree))
                                 (eq cdr (cdr subtree)))
                            subtree
                            (cons car cdr)))))))
    (s tree)))

(defun layout-literal-str (forms)
  (let* ((strs (make-hash-table :test 'equal))
         (forms (walk-tree forms #'stringp (lambda (str) (alexandria:if-let (sym (gethash str strs))
                                                      (intern (format nil "=~a" sym))
                                                      (let ((sym (gensym)))
                                                        (setf (gethash str strs) sym)
                                                        (intern (format nil "=~a" sym)))))))
         data)
    (maphash (lambda (str sym)
               (push sym data)
               (push (list '.ascii str) data))
             strs)
    (append forms '((.data)) (nreverse data))))

(defun assemble-value (value)
  (typecase value
    (number (format nil "~a" value))
    (string (format nil "\"~a\"" (unescape-as-cstr value)))
    (otherwise value)))

(defun unescape-as-cstr (str)
  (with-output-to-string (ret)
    (loop for char across str
          do (progn (if (char= #\Newline char)
                        (format ret "~a~a" #\\ #\n)
                        (write-char char ret))))))

(defparameter *gates* nil)

(defparameter *used* nil)

(defun compile-forms (forms)
  (mapcan #'compile-form forms))

(defun compile-form (form)
  (case (car form)
    (cpu (cdr form))
    (defgate (process-defgate (cdr form)))
    (use (process-use (cdr form)))
    (t (apply-gate form))))

(defun process-use (form)
  (unless (gethash (car form) *used*)
    (compile-forms (parse-file (car form)))))

(defun apply-gate (form)
  (funcall (gethash (car form) *gates*) (cdr form)))

(defun process-defgate (form)
  ;; TODO: replace lisp eval to coolgate's eval to self host
  (setf (gethash (car form) *gates*) (eval (cadr form)))
  ())

(defun parse-file (filepath)
  (with-open-file (s filepath)
    (named-readtables:in-readtable trivial-escapes:dq-readtable)
    (do ((form (read s nil :eof) (read s nil :eof))
         ret)
        ((eql form :eof) (nreverse ret))
      (push form ret))))

(defparameter *assembler* "aarch64-linux-gnu-as")

(defparameter *linker* "aarch64-linux-gnu-ld")

(defun compile-coolgate-file (filepath)
  (let ((forms (parse-file filepath))
        (*gates* (make-hash-table))
        (*used* (make-hash-table :test 'equal))
        (asmpath (namestring (make-pathname :defaults filepath :type "s")))
        (objpath (namestring (make-pathname :defaults filepath :type "o")))
        (exepath (namestring (make-pathname :defaults filepath :type nil))))
    (with-open-file (f asmpath :direction :output :if-exists :supersede)
      (princ (princ (assemble-forms (layout-literal-str (compile-forms forms)))) f))
    (multiple-value-bind (out err ret)
        (uiop:run-program (print `(,*assembler* ,asmpath "-o" ,objpath)) :error-output :string :ignore-error-status t)
      (print out)
      (print err)
      (print ret)
      (when (/= 0 ret)
        (error err))
      (multiple-value-bind (out err ret)
          (uiop:run-program `(,*linker* ,objpath "-o" ,exepath) :error-output :string :ignore-error-status t)
        (when (/= 0 ret)
          (error err))))))
