(in-package :cl-user)
(in-package :cl-user)
(defpackage :coolgate-arm64
  (:use :cl))
(in-package :coolgate-arm64)

(defun unescape-cstr (str)
  (with-output-to-string (ret)
    (loop for char across str
          do (progn (if (char= #\Newline char)
                        (format ret "~a~a" #\\ "n")
                        (write-char char ret))))))

(defun compile-form (form code data)
  (cond ((listp form)
         (case (car form)
           ((write-stdout) (compile-write-stdout (compile-form (cadr form) code data) code data))
           ((exit) (compile-exit (compile-form (cadr form) code data) code data))
           (otherwise (error "unknown args"))))
        ((stringp form) (compile-string-literal form code data))
        ((integerp form) form)))

(defun compile-forms (forms)
  (with-output-to-string (asm)
    (format asm ".global _start~%")
    (format asm "._start:~%")
    (format asm ".data~%~a~%"
            (with-output-to-string (data)
              (format asm "~a~%"
                      (with-output-to-string (code)
                        (loop for form in forms do (compile-form form code data))))))))

(defun compile-exit (exitcode code data)
  (format code "mov X0, #~a
mov X8, #93
svc 0
" exitcode))

(defun compile-write-stdout (str code data)
  (format code "
mov X0, #1
ldr X1, =~a
mov X2, #~b
mov X8, #64
svc 0
" (car str) (cdr str)))

(defun compile-string-literal (str code data)
  (let ((strname (gensym)))
    (format data "
~a: .ascii \"~a\"
" strname (unescape-cstr str))
    (cons strname (length str))))
