(in-package :prolog-ext)

(defun ensure-slot-specifiers (slot-specifiers)
  (mapcar #'(lambda (slot-specifier)
              (cond ((symbolp slot-specifier)
                     (list (ensure-slot-name slot-specifier)))
                    ((consp slot-specifier)
                     (ensure-slot-specifier slot-specifier))))
          slot-specifiers))

(defun ensure-slot-specifier (slot-specifier)
  (destructuring-bind (slot-name &key selector) slot-specifier
    (list (ensure-slot-name slot-name)
          selector)))

(defun ensure-slot-name (slot-name)
  (prog1 slot-name
    (unless (eql #\? (aref (string slot-name) 0))
      (error "slot-name ~A does not begin with ?" slot-name))))

(defun slot-name (slot-specifier)
  (first slot-specifier))

(defun slot-selector (slot-specifier)
  (second slot-specifier))

(defun collect-slot-names (slot-specifiers)
  (mapcar #'slot-name slot-specifiers))

(defun collect-slot-selectors (slot-specifiers)
  (mapcar #'slot-selector slot-specifiers))

(defun slot-selector-terms (struct-name slot-selectors slot-names)
  (loop for slot-selector in slot-selectors
        for slot-name in slot-names
        when slot-selector
        collect (slot-selector-term struct-name slot-selector slot-name
                                    slot-names)))

(defun slot-selector-term (struct-name slot-selector slot-name slot-names)
  (flet ((slot-names ()
           (mapcar #'(lambda (x)
                       (if (eq x slot-name) slot-name '?))
                   slot-names)))
  ` (<-- (,slot-selector (,struct-name ,@(slot-names)) ,slot-name))))

