(in-package :cl-user)

(defpackage :uncommon-lisp
  (:nicknames :ul)
  ;; (:use :common-lisp :iterate)
  (:use :common-lisp)
  (:export ;;; The following come from Paul Graham's _On Lisp_.
           ;; Section 4.3
           :last1 :single :append1 :conc1 :mklist :longer :filter
           :group :prune
           ;; Section 4.4, Utility Functions: Search
           :find2 :before :after :duplicate :split-if :most :best
           :mostn
           ;; Section 4.5, Utility Functions: Mapping
           :map0-n :map1-n :mapa-b :map-> :mappend :mapcars :rmapcar
           ;; Section 5.4, Returning Functions: Composing Functions
           :compose :fif :fint :fun
           ;; Section 5.5, Returning Functions: Recursion on Cdrs
           :lrec
           ;; Section 11.1, Classic Macros: Creating Context
           :when-bind :when-bind* :with-gensyms :condlet
           ;; Section 14.1, Anaphoric Macros: Anaphoric Variants
           :it :aif :awhen :awhile :aand :acond :alambda :ablock
           :aif2 :awhen2 :acond2 :read2 :do-file
           ;; Section 15.1, Macros Returning Functions: Building Functions
           :fn :rbuild
           ;; Section 15.1, Macros Returning Functions: Recursion on CDRS
           :alrec :on-cdrs :unions :intersections :differences :maxmin
           ;;; The following come from Peter Siebel's _Practical
           ;;; Common Lisp_.
           :list-directory :file-exists-p :directory-pathname-p
           :file-pathname-p :walk-directory :directory-p :file-p
           ;; The following two already exist in the EXCL package
           :pathname-as-directory :pathname-as-file
           ;;; The following are Damien Kick's silly, miscellaneous
           ;;; utilities.
           :*default-sh-output*
           :use :est :with-fns :import-bound-symbols :slurp :sh
           :with-open-files :with-open-dir :with-open-dirs :join
           :asdf-load
           ;; Damien Kick's reader macro stuff
           :in-syntax-kick-reader-macros
           :|!( READER| :|!' READER| :|!` READER| :|!$ READER|))

#+allegro
(defpackage :prolog-ext
  (:use :common-lisp :uncommon-lisp :prolog)
  (:export :define-prolog-struct))

#+allegro
(defpackage :prolog-user
  (:use :common-lisp :uncommon-lisp :prolog :prolog-ext))

(defpackage :uncommon-lisp-user
  (:nicknames :ul-user)
  ;; (:use :common-lisp :uncommon-lisp :iterate :cl-ppcre)
  (:use :common-lisp :uncommon-lisp :cl-ppcre))
