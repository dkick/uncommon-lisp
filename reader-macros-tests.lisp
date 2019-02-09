(in-package #:pg-user)

(in-syntax-kick-reader-macros)

(defun test0 ()
  !'(UPPER lower CamelCase))

(defun test1 ()
  (let ((x 69))
    !`(x ,X y z)))

(defun test2 ()
  (sh !'(ls)))

(defun test3 ()
  !$(ls \-l))

(defun test4 ()
  !(LIST 'x 1 2 3))
