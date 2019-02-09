(in-package :prolog-ext)

(defmacro define-prolog-struct
    (name (&rest super-structs) raw-slot-specifiers)
  (when super-structs (error "Not ready to handle inheritance."))
  (let* ((slot-specifiers (ensure-slot-specifiers raw-slot-specifiers))
         (slot-names (collect-slot-names slot-specifiers))
         (slot-selectors (collect-slot-selectors slot-specifiers)))
    ` (progn
        (<-- (,name ,@slot-names))
        ,@(slot-selector-terms name slot-selectors slot-names))))

