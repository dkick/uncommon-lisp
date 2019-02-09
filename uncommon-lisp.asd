; -*- lisp -*-

(cl:defpackage :uncommon-lisp.system
  (:use :common-lisp :asdf))

(cl:in-package :uncommon-lisp.system)

#+allegro
(cl:require :excl)

(asdf:oos 'asdf:load-op :cl-ppcre)

(defsystem :uncommon-lisp
  ;; :depends-on (:cl-ppcre :iterate)
  :depends-on (:cl-ppcre)
  :components ((:file "packages")
               (:file "on-lisp-from-paul-graham"
                      :depends-on ("packages"))
               (:file "misc"
                      :depends-on ("packages"
                                   "on-lisp-from-paul-graham"))
               (:file "reader-macros"
                      :depends-on ("packages"
                                   "on-lisp-from-paul-graham"
                                   "misc"))
               (:file "pathnames"
                      :depends-on ("packages"))
               #+allegro
               (:file "prolog-struct-functions"
                      :depends-on ("packages"))
               #+allegro
               (:file "prolog-ext-macros"
                      :depends-on ("packages"
                                   "prolog-struct-functions"))))
