;;;; Mine very own utilities...

(in-package #:uncommon-lisp)

;;; Macros...

(defmacro use (&body forms)
  "  (USE F ARGS) => (F ARGS)
What's the point?  Indents as
  (USE F-IS-REALLY-THE-NAME-OF-AN-OBNOXIOUSLY-NAMED-FUNCTION
       ARGS)
Instead of
  (F-IS-REALLY-THE-NAME-OF-AN-OBNOXIOUSLY-NAMED-FUNCTION
   ARGS)
This looks more like function call indentation.  Also
  (USE (F G H) X Y Z) => (F (G (H X Y Z)))"

  (cond ((listp (car forms))
         (reduce #'(lambda (arg accumulator)
                     `(,arg ,accumulator))
                 (butlast (car forms))
                 :from-end t
                 :initial-value `(,@(last (car forms)) ,@(cdr forms))))
        (t `(,@forms))))

(defmacro est (symbol &body forms)
  "Est. (establish): similar to defparameter.  Establish \"name\" as a
parameter with the value of \"forms\".  However, it returns the
values: value, name.  This form also insists on the symbol having been
named with the convention for special variables."
  (let ((name (symbol-name symbol)))
    (unless (and (eql (schar name 0) #\*)
                 (eql (schar name (1- (length name))) #\*))
      (error "~S is not a valid *name*" symbol))
    (unless (eql (symbol-package symbol) *package*)
      (error "~S comes from ~S" symbol (symbol-package symbol))))
  `(progn (defparameter ,symbol ,@forms) (values ,symbol ',symbol)))

(defmacro with-open-files ((&rest args) &body forms)
  "Multiple WITH-OPEN-FILE in a LETish syntax."
  (reduce #'(lambda (arg accumulator)
              `(with-open-file ,arg ,accumulator))
          args :from-end t :initial-value `(progn ,@forms)))

#-(and)
(defmacro with-open-dir ((dir dir-name) &body forms)
  "Like WITH-OPEN-FILE but protecting UNIX:OPEN-DIR with UNIX:CLOSE-DIR."
  ` (let ((,dir (unix:open-dir ,dir-name)))
      (unwind-protect
           (progn ,@forms)
        (when ,dir
          (unix:close-dir ,dir)))))

#-(and)
(defmacro with-open-dirs ((&rest args) &body forms)
  "Multiple WITH-OPEN-DIR in a LETish syntax."
  (reduce #'(lambda (arg accumulator)
              `(with-open-dir ,arg ,accumulator))
          args :from-end t :initial-value `(progn ,@forms)))

(defmacro with-gensyms ((&rest args) &body forms)
  "Like Paul Graham's WITH-GENSYMS but generates a gensym with a hint
for the name."
  `(let ,(mapcar #'(lambda (var)
                     `(,var (gensym (concatenate 'string
                                                 (string ',var) "/"))))
                 args)
     ,@forms))

;; I got this from <http://tinyurl.com/yknams>.
(defmacro with-fns (bindings &body body)
  "Bind each variable in BINDINGS as a function, so you can use Lisp-1
notation - e.g. (with-fns (fn) (fn 3)) is the same as (funcall fn 3).
Each binding is either the name of the function or an (ALIAS
ORIG-NAME) list."
  (labels ((pairify (binding)
             (if (consp binding) binding (list binding binding))))
    ` (labels ,(loop for (name fn) in (mapcar #'pairify bindings)
                     collect `(,name (&rest args)
                               (apply ,fn args)))
        ,@body)))

;;; Functions and the special variables they use...

(defun import-bound-symbols (source-package
                             &optional (target-package *package*))
  "Import all of the symbols from SOURCE-PACKAGE which are either
FBOUNDP or BOUNDP into TARGET-PACKAGE (defaults to *PACKAGE*)."
  (loop for there being each present-symbol of source-package
        for here = (find-symbol (symbol-name there) target-package)
        when (funcall (fn (or fboundp boundp)) there)
            if here do (unless (eq there here)
                         (format t "~&~A has ~S~%"
                                 (package-name target-package) here))
            else do (import (list there) target-package)
                 and collect there))

(defun slurp (file-spec)
  "Slurp up all of the file, return it as a string.  Perhaps READ-FILE
would've been a better name."
  (with-open-file (file-stream file-spec :direction :input)
    (with-output-to-string (buffer)
      (loop for char = (read-char file-stream nil)
            while char do (write-char char buffer)))))

(defvar *default-sh-output* nil
  "The stream to which SH should direct it's ouput.  NIL means that
the output should be returned as a string.  T is synonymous with
*STANDARD-OUTPUT*")

#+sbcl
(defun sh (command &optional (output *default-sh-output*))
  "Process elements of COMMAND with format control string ~A as
arguments to be passed to RUN-PROGRAM.  OUTPUT, which defaults to
*DEFAULT-SH-OUPUT* is either a stream (T is synonymous with
*STANDARD-OUTPUT*) to which the program output will be written or NIL,
in which case the output will be returned as a STRING.  When OUTPUT is
a STREAM, the return values are that STREAM and the PROCESS.  When
OUTPUT is a string, the return values are the STRING and the PROCESS.
In all cases, the returned PROCESS has been passed to PROCESS-CLOSE
before being returned."
  (labels ((lemma (command output)
             (let ((command-strings (mapcar #'(lambda (element)
                                                (format nil "~A" element))
                                            command)))
               ;; Does it make sense to wrap any of the following in
               ;; an UNWIND-PROTECT?
               (awhen (sb-ext:run-program (car command-strings)
                                          (cdr command-strings)
                                          :output output :search t)
                 (sb-ext:process-close it)))))
    (let ((output (if (eq output t) *standard-output* output)))
      (etypecase output
        (null
         (let ((buffer (make-array '(0) :element-type 'base-char
                                   :fill-pointer 0 :adjustable t)))
           (with-output-to-string (output buffer)
             (aif (lemma command output)
                  (values buffer it)
                  (values nil nil)))))
        (stream
         (values output (lemma command output)))))))

#+allegro
(defun sh (command &optional (output *default-sh-output*))
  "This function makes more sense for SBCL or CMUCL.  Basically, it
just calls (command-output (format nil \"~{~A ~}\" command) :whole t)"
  (declare (ignore output))
  (when *default-sh-output*
    ;; I really only ever use this to return a string anyway.
    (error "This implementation only supports output to a string."))
  (excl.osi:command-output (format nil "~{~A ~}" command) :whole t))

(defun join (strings glue &optional stream)
  "cl-ppcre has a split but no join.  This is a version of join.  One
can find faster implementations with Google, but this works."
  (flet ((lemma (strings1 glue1 stream1)
           (loop for (car . cdr) on strings1
                 do (write-string car stream1)
                    (when cdr
                      (write-string glue1 stream1)))))
    (if stream
        (lemma strings glue stream)
        (with-output-to-string (result)
          (lemma strings glue result)))))

(defun asdf-load (system &rest args)
  (apply #'asdf:oos 'asdf:load-op system args))
