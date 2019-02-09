;;; This code is copyright 1993 by Paul Graham.
;;; Distribution under clocc.sourceforge.net
;;; Liam M. Healy (LMH), liam@users.sourceforge.net
;;; By permission of Paul Graham, this is distributed under the
;;; GNU General Public License (GPL), version 2.
;;; The license accompanies this file under the name "COPYING", or
;;; see http://www.gnu.org/copyleft/gpl.html for a copy.

;;; Damien Kick changes are:
;;;  - different package, uncommon-lisp
;;;  - only include a subset of the functions
;;; LMH changes are:
;;; Added
;;;  - in-package and export defintions from this package
;;;  - form seperators (by book section)
;;;  - comments
;;;  - some type declarations (originally to help CMUCL.
;;;  - macro #'defmemoize.
;;;  - (declare (ignorable it)) for the anaphoric things.
;;;  - added eval-when to ddfn to supress compile-warning
;;;  - CLtL1 conditionalization for alrec/atrec.
;;; Removed
;;;  - multiple definitions (see below)
;;;  - #\{ delimiter - messes up emacs
;;; Changed
;;;  - get-setf-method to ANSI's get-setf-expansion (conditionally)

;;; Book Info:
;;; Paul Graham, On Lisp, Prentice-Hall 1994
;;; ISBN:  0-13-030552-9
;;; LC: QA 76.73.C28G73 1994

;;; This file is utilities (Chapters 1-18).  An effort has been made to insure
;;; only the best definition (if multiple definintions were given) actually
;;; will be read.

(in-package #:uncommon-lisp)

;;;; Section 4.3, Utility Functions: Operations on Lists

(defun last1 (lst)
  "The last item in a list."            ; LMH
  (declare (list lst))
  (car (last lst)))

(defun single (lst)
  "Test list for one element."          ; LMH
  (and (consp lst) (not (cdr lst))))

(defun append1 (lst obj)
  "Add the obj onto the end of the lst." ; LMH
  (append lst (list obj)))

(defun conc1 (lst obj)
  "Destructively add the obj onto the end of the lst." ; LMH
  (nconc lst (list obj)))

(defun mklist (obj)
  "Make a list out of the object if it isn't already." ; LMH
  (if (listp obj) obj (list obj)))

(defun longer (x y)
  "Test if list x is longer than list y.  More efficient
   than calling length twice."          ; LMH
  (declare (sequence x y))              ; LMH
  (labels ((compare (x y)
             (and (consp x)
                  (or (null y)
                      (compare (cdr x) (cdr y))))))
    (if (and (listp x) (listp y))
        (compare x y)
        (> (length x) (length y)))))

(defun filter (fn lst)
  "Calls fn on each element of the lst, returning a
   list of non-NIL results."            ; LMH
  (declare (function fn))               ; LMH
  (let ((acc nil))
    (dolist (x lst)
      (let ((val (funcall fn x)))
        (if val (push val acc))))
    (nreverse acc)))

(defun group (source n)
  "Group source list into sublists of length n."   ; LMH
  (declare (fixnum n))                  ; LMH
  (if (zerop n) (error "zero length"))
  (labels ((rec (source acc)
             (let ((rest (nthcdr n source)))
               (if (consp rest)
                   (rec rest (cons (subseq source 0 n) acc))
                   (nreverse (cons source acc))))))
    (if source (rec source nil) nil)))

(defun prune (test tree)
  "Descend tree, removing items that return nil from test."	; LMH
  (declare (function test))   ; LMH
  (labels ((rec (tree acc)
             (cond ((null tree) (nreverse acc))
                   ((consp (car tree))
                    (rec (cdr tree)
                         (cons (rec (car tree) nil) acc)))
                   (t (rec (cdr tree)
                           (if (funcall test (car tree))
                               acc
                               (cons (car tree) acc)))))))
    (rec tree nil)))

;;;; Section 4.4, Utility Functions: Search

(defun find2 (fn lst)
  "Find the first element of lst that satisfies fn, returning
   both the element and result of fn.  Like find, but also returns
   the result of calling fn."   ; LMH
  (declare (function fn))   ; LMH
  (if (null lst)
      nil
      (let ((val (funcall fn (car lst))))
        (if val
            (values (car lst) val)
            (find2 fn (cdr lst))))))

(defun before (x y lst &key (test #'eql))
  "Test if x occurs before y in lst.
   Returns true if second element doesn't occur at all."   ; LMH
  (declare (function test))   ; LMH
  (and lst
       (let ((first (car lst)))
         (cond ((funcall test y first) nil)
               ((funcall test x first) lst)
               (t (before x y (cdr lst) :test test))))))

(defun after (x y lst &key (test #'eql))
  "Test if x occurs after y in lst."   ; LMH
  (declare (function test))   ; LMH
  (let ((rest (before y x lst :test test)))
    (and rest (member x rest :test test))))

(defun duplicate (obj lst &key (test #'eql))
  "Test if obj is duplicated in lst."   ; LMH
  (declare (function test))   ; LMH
  (member obj (cdr (member obj lst :test test))
          :test test))

(defun split-if (fn lst)
  "Split the list into two at the first element that satisfies fn."   ; LMH
  (declare (function fn))   ; LMH
  (let ((acc nil))
    (do ((src lst (cdr src)))
        ((or (null src) (funcall fn (car src)))
         (values (nreverse acc) src))
      (push (car src) acc))))

(defun most (scoring-fn lst)
  "Return the element and the score that returns the highest
   values of the scoring-fn.  Scoring-fn is a function that
   returns a fixnum."   ; LMH
  (declare (function scoring-fn))			; LMH
  (if (null lst)
      (values nil nil)
      (let* ((wins (car lst))
             (max (funcall scoring-fn wins)))
	(declare (fixnum max))			; LMH scoring-fn must return fixnum
        (dolist (obj (cdr lst))
          (let ((score (funcall scoring-fn obj)))
	    (declare (fixnum score))		; LMH scoring-fn must return fixnum
            (when (> score max)
              (setq wins obj
                    max  score))))
        (values wins max))))

(defun best (predicate lst)
  "The element of lst for which the predicate always returns t when called
   with other elements in lst, like (car (sort lst predicate)) but
   potentially more efficient."   ; LMH
  (declare (function predicate))   ; LMH
  (if (null lst)
      nil
      (let ((wins (car lst)))
        (dolist (obj (cdr lst))
          (if (funcall predicate obj wins)
              (setq wins obj)))
        wins)))

(defun mostn (scoring-fn lst)
  "Return a list of all elements and the score that return the highest
   values of the scoring-fn.  Scoring-fn is a function that
   returns a fixnum."   ; LMH
  (declare (function scoring-fn))			; LMH
  (if (null lst)
      (values nil nil)
      (let ((result (list (car lst)))
            (max (funcall scoring-fn (car lst))))
	(declare (fixnum max))			; LMH scoring-fn must return fixnum
        (dolist (obj (cdr lst))
          (let ((score (funcall scoring-fn obj)))
	    (declare (fixnum score))		; LMH scoring-fn must return fixnum
            (cond ((> score max)
                   (setq max    score
                         result (list obj)))
                  ((= score max)
                   (push obj result)))))
        (values (nreverse result) max))))

;;;; Section 4.5, Utility Functions: Mapping

(defun map0-n (fn n)
  (declare (function fn) (fixnum n))   ; LMH
  "Apply the fn to the list of numbers 0...n."  ; LMH
  (mapa-b fn 0 n))

(defun map1-n (fn n)
  (declare (function fn) (fixnum n))   ; LMH
  "Apply the fn to the list of numbers 1...n."  ; LMH
  (mapa-b fn 1 n))

(defun mapa-b (fn a b &optional (step 1))
  (declare (function fn) (fixnum a b step))   ; LMH
  "Apply the fn to the list of numbers a...b, stepping with step."  ; LMH
  (do ((i a (+ i step))
       (result nil))
      ((> i b) (nreverse result))
    (declare (fixnum i))   ; LMH
    (push (funcall fn i) result)))

(defun map-> (fn start test-fn succ-fn)
  (declare (function fn test-fn succ-fn))	; LMH
  "Apply fn to each member of the generated sequence whose
  first element is start, succesive elements are generated
  with succ-fn, and whose last element is the last element
  that does not satisify test-fn."   ; LMH
  (do ((i start (funcall succ-fn i))
       (result nil))
      ((funcall test-fn i) (nreverse result))
    (declare (fixnum i))			; LMH
    (push (funcall fn i) result)))

(defun mappend (fn &rest lsts)
  (declare (function fn))   ; LMH
  "Nondestructive form of mapcan."   ; LMH
  (apply #'append (apply #'mapcar fn lsts)))

(defun mapcars (fn &rest lsts)
  (declare (function fn))			; LMH
  "Mapcar a function over several lists, like (apply #'mapcar fn lsts) but without
   unnecessary consing."   ; LMH
  (let ((result nil))
    (dolist (lst lsts)
      (dolist (obj lst)
        (push (funcall fn obj) result)))
    (nreverse result)))

(defun rmapcar (fn &rest args)
  (declare (function fn))			; LMH
  "Mapcar for trees."				; LMH
  (if (some #'atom args)
      (apply fn args)
      (apply #'mapcar
             #'(lambda (&rest args)
                 (apply #'rmapcar fn args))
             args)))

;;;; Section 5.4, Returning Functions: Composing Functions

(defun compose (&rest fns)
  "Compose the functions."              ; LMH
  (if fns
      (let ((fn1 (car (last fns)))
            (fns (butlast fns)))
	(declare (function fn1))        ; LMH
        #'(lambda (&rest args)
            (reduce #'funcall fns
                    :from-end t
                    :initial-value (apply fn1 args))))
      #'identity))

(defun fif (if then &optional else)
  (declare (function if then) (type (or function null) else)) ; LMH
  "Function-if: create a function that, if if is true on its
   argument, then return then called on the argument, otherwise
   else called on the argument (or nil)." ; LMH
  #'(lambda (x)
      (if (funcall if x)
          (funcall then x)
          (if else (funcall else x)))))

(defun fint (fn &rest fns)
  (declare (function fn))               ; LMH
  "Function intersection: a function that is the
   AND of each function called on the argument." ; LMH
  (if (null fns)
      fn
      (let ((chain (apply #'fint fns)))
	(declare (function chain))      ; LMH correct?
        #'(lambda (x)
            (and (funcall fn x) (funcall chain x))))))

(defun fun (fn &rest fns)
  (declare (function fn))               ; LMH
  "Function union: a function that is the
   OR of each function called on the argument." ; LMH
  (if (null fns)
      fn
      (let ((chain (apply #'fun fns)))
	(declare (function chain))      ; LMH correct?
        #'(lambda (x)
            (or (funcall fn x) (funcall chain x))))))

;;;; Section 5.5, Returning Functions: Recursion on Cdrs

(defun lrec (rec &optional base)
  (declare (function rec))			; LMH
  "Function to define flat list recurser.
   Rec is a function that takes two arguments, the first
   will be the car of the list, the second is a function
   that will produce the cdr.  Base is a function to be called
   or value when the list is empty.
   For example, a length function could be defined as
   (lrec #'(lambda (x f) (1+ (funcall f))) 0)."   ; LMH
  (labels ((self (lst)
             (if (null lst)
                 (if (functionp base)
                     (funcall base)
                     base)
                 (funcall rec (car lst)
                              #'(lambda ()
                                  (self (cdr lst)))))))
    #'self))

;;;; Section 11.1, Classic Macros: Creating Context

(defmacro when-bind ((var expr) &body body)
  "Like a single let binding, but body is not executed if var is bound to nil"   ; LMH
  `(let ((,var ,expr))
     (when ,var
       ,@body)))

(defmacro when-bind* (binds &body body)
  "Like let*, but body is not executed if var is bound to nil"   ; LMH
  (if (null binds)
      `(progn ,@body)
      `(let (,(car binds))
         (if ,(caar binds)
             (when-bind* ,(cdr binds) ,@body)))))

;; The following is the definition from _On Lisp_ but I use a slightly
;; different variation which can be found in "misc.lisp".
#-(and)
(defmacro with-gensyms (syms &body body)
  "Create gensyms, useful for creating macros."   ; LMH
  `(let ,(mapcar #'(lambda (s)
                     `(,s (gensym)))
                 syms)
     ,@body))

(defmacro condlet (clauses &body body)
  "Conditional let.  Example:
   (condlet (((= 1 2) (x (princ 'a)) (y (princ 'b)))
             ((= 1 1) (x (princ 'c)) (y (princ 'd)))
             (t       (x (princ 'e)) (y (princ 'f))))
     (list x y z))
  CD
  (D C NIL)."   ; LMH
  (let ((bodfn (gensym))
        (vars (mapcar #'(lambda (v) (cons v (gensym)))
                      (remove-duplicates
                        (mapcar #'car
                                (mappend #'cdr clauses))))))
    `(labels ((,bodfn ,(mapcar #'car vars)
                 ,@body))
       (cond ,@(mapcar #'(lambda (cl)
                           (condlet-clause vars cl bodfn))
                       clauses)))))

(defun condlet-clause (vars cl bodfn)
  `(,(car cl) (let ,(mapcar #'cdr vars)
                (let ,(condlet-binds vars cl)
                  (,bodfn ,@(mapcar #'cdr vars))))))


(defun condlet-binds (vars cl)
  (mapcar #'(lambda (bindform)
              (if (consp bindform)
                  ;; LMH the symbol
                  (cons (cdr (assoc (the symbol (car bindform)) vars))
                        (cdr bindform))))
          (cdr cl)))

;;;; Section 14.1, Anaphoric Macros: Anaphoric Variants

(defmacro aif (test-form then-form &optional else-form)
  "Anaphoric if: use `it' in then-form, else-form to
   refer to result of the test-form."   ; LMH
  `(let ((it ,test-form))
     (declare (ignorable it))		; LMH
     (if it ,then-form ,else-form)))

(defmacro awhen (test-form &body body)
  "Anaphoric when: use `it' in body to
   refer to result of the test-form."   ; LMH
  `(aif ,test-form
        (progn ,@body)))

(defmacro awhile (expr &body body)
  "Anaphoric while: use `it' in body to
   refer to result of the expr"		; LMH
  `(do ((it ,expr ,expr))
       ((not it))
     (declare (ignorable it))		; LMH
     ,@body))

(defmacro aand (&rest args)
  "Anaphoric and: use `it' to refer to result of
  previous form evaluation."   ; LMH
  (cond ((null args) t)
        ((null (cdr args)) (car args))
        (t `(aif ,(car args) (aand ,@(cdr args))))))

(defmacro acond (&rest clauses)
  "Anaphoric cond: use `it' in consequent to refer to result of test."    ; LMH
  (if (null clauses)
      nil
      (let ((cl1 (car clauses))
            (sym (gensym)))
        `(let ((,sym ,(car cl1)))
           (if ,sym
               (let ((it ,sym))
		 (declare (ignorable it)) ; LMH
		 ,@(cdr cl1))
               (acond ,@(cdr clauses)))))))

(defmacro alambda (parms &body body)
  "Anaphoric lambda: use `self' in body to refer to this lambda."   ; LMH
  `(labels ((self ,parms ,@body))
     #'self))

(defmacro ablock (tag &rest args)
  "Anaphoric block: use `it' in to refer to previous expression." ; LMH
  `(block ,tag
     ,(funcall (alambda (args)
			(cond
			 ((null args) nil)
			 ((single args) (car args))
			 (t `(let ((it ,(car args)))
			       (declare (ignorable it)) ; LMH
			       ,(self (cdr args))))))
	       args)))

;;; LMH the 2 versions are useful for e.g. gethash returning two values:

(defmacro aif2 (test &optional then else)
  "Anaphoric if for two returned values: use `it' in `then', `else' clauses
   to refer to result of the test.  Test can return two values,
   if is satisfied if either is true, but `it' refers to first." ; LMH
  (let ((win (gensym)))
    `(multiple-value-bind (it ,win)
	 ,test
       (if (or it ,win) ,then ,else))))

(defmacro awhen2 (test &body body)
  "Anaphoric when for two returned values: use `it' in body to
   refer to result of the test.  Test can return two values,
   when is satisfied if either is true, but `it' refers to first."   ; LMH
  `(aif2 ,test
         (progn ,@body)))

(defmacro acond2 (&rest clauses)
  "Anaphoric cond for two returned values: use `it' in body to
   refer to result of the test.  Test can return two values,
   when is satisfied if either is true, but `it' refers to first."   ; LMH
  (if (null clauses)
      nil
      (let ((cl1 (car clauses))
            (val (gensym))
            (win (gensym)))
        `(multiple-value-bind (,val ,win) ,(car cl1)
           (if (or ,val ,win)
               (let ((it ,val))
		 (declare (ignorable it)); LMH
		 ,@(cdr cl1))
               (acond2 ,@(cdr clauses)))))))

(let ((g (gensym)))
  (defun read2 (&optional (str *standard-input*))
  "Read, returning a second value showing something read,
   otherwise nil."   ; LMH
    (let ((val (read str nil g)))
      (unless (equal val g) (values val t)))))

(defmacro do-file (filename &body body)
  "Iterate over file, reading forms."   ; LMH
  (let ((str (gensym)))
    `(with-open-file (,str ,filename)
       ;; The CLOCC version used AWHILE2 which does not exist?
       (awhile (read2 ,str)
         ,@body))))

;;;; Section 15.1, Macros Returning Functions: Building Functions

(defmacro fn (expr)
  "Make the function according to the expression,
   e.g., (fn (and ingegerp oddp)) makes the function
   #'(lambda (x) (and (integerp x) (oddp x)))."   ; LMH
 `#',(rbuild expr))

(defun rbuild (expr)
  (if (or (atom expr) (eq (car expr) 'lambda))
      expr
      (if (eq (car expr) 'compose)
          (build-compose (cdr expr))
          (build-call (car expr) (cdr expr)))))

(defun build-call (op fns)
  (let ((g (gensym)))
    `(lambda (,g)
       (,op ,@(mapcar #'(lambda (f)
                          `(,(rbuild f) ,g))
                      fns)))))

(defun build-compose (fns)
  (let ((g (gensym)))
    `(lambda (,g)
       ,(labels ((rec (fns)
                   (if fns
                       `(,(rbuild (car fns))
                         ,(rec (cdr fns)))
                       g)))
          (rec fns)))))

(defmacro alrec (rec &optional base)
  "Anaphoric list recurser (lrec): use `it' to refer to the current
   car of the list, and `rec' to the function rec itself.
   every on #'oddp,
   (alrec (and (oddp it) rec) t) is the equivalent of
   (lrec #'(lambda (x f) (and (oddp x) (funcall f))) t)."   ; LMH
  (let ((gfn (gensym)))
    `(lrec #'(lambda (it ,gfn)
	       (declare (ignorable it) (function ,gfn))   ; LMH
               (symbol-macrolet ((rec (funcall ,gfn)))
                 ,rec))
           ,base)))

(defmacro on-cdrs (rec base &rest lsts)
  ;; LMH
  "Anaphoric list recursion, for defining named functions,
   e.g., (defun our-every (fn lst) (on-cdrs (and (funcall fn it) rec) t lst))."
  ;; LMH: the function
  `(funcall (the function (alrec ,rec #'(lambda () ,base))) ,@lsts))

(defun unions (&rest sets)
  "The union of all the sets (like union, but takes an arbitrary number of arguments)."   ; LMH
  (on-cdrs (union it rec) (car sets) (cdr sets)))

(defun intersections (&rest sets)
  "The intersection of all the sets (like intersection,
   but takes an arbitrary number of arguments)."   ; LMH
  (unless (some #'null sets)
    (on-cdrs (intersection it rec) (car sets) (cdr sets))))

(defun differences (set &rest outs)
  "The set difference of all the sets (like set-difference,
   but takes an arbitrary number of arguments).  Handles multiple
   arguments the same way #'- does."   ; LMH
  (on-cdrs (set-difference rec it) set outs))

(defun maxmin (args)
  "Finds both the maximum and minimum in a single traversal of the list."   ; LMH
  (when args
    (on-cdrs (multiple-value-bind (mx mn) rec
	       (declare (number mx mn))   ; LMH
               (values (max mx it) (min mn it)))
             (values (car args) (car args))
             (cdr args))))
