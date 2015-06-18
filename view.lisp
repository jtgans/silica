(in-package :silica)

(defclass view (handler)
  ((rect
    :initform nil
    :type (or rect null)
    :accessor view-rect
    :initarg :rect
    :documentation
    "The space which this view takes up.")
   (parent-view
    :initform nil
    :type (or view null)
    :accessor view-parent
    :initarg :parent-view
    :documentation
    "The parent of this view.")
   (child-views
    :initform nil
    :type list
    :accessor view-children
    :initarg :child-views
    :documentation
    "A list of views that exist as children inside this view.")
   (keymap
    :initform nil
    :type hashtable
    :accessor view-keymap
    :initarg :keymap
    :documentation
    "A sub-keymap used to dispatch key events to handlers.")
   (visible
    :initform nil
    :type boolean
    :reader view-visible-p
    :initarg :visible
    :documentation
    "A simple boolean to indicate if this view is visible on screen or not."))
  (:documentation
   "Represents a widget on the screen."))

(defmethod print-object ((object view) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (rect parent-view child-views keymap) object
      (format stream ":RECT ~a :PARENT ~(~A~) :CHILD-VIEWS ~(~A~) :KEYMAP ~a"
              rect parent-view child-views keymap))))

;;;; Default methods and functions

(defmethod handle-event ((view view) (event layout-event))
  "Default LAYOUT-EVENT method that simply sets the VIEW-RECT for this VIEW to
the LAYOUT-RECT given in the LAYOUT-EVENT."
  (setf (view-rect view) (layout-rect layout-event))
  (post-event view (make-instance 'draw-event :sender view)))

(defmethod handle-event ((view view) (event draw-event))
  "Default draw method that loops through all child views and draws them by
recursively calling VIEW-DRAW."
  (with-slots (child-views visible) view
    (loop for child-view in child-views do
         (view-draw child-view))))

(defun view-set-visibility (view visibility &key include-children)
  "Sets the given VIEW's visibility to VISIBILITY. Also applies to children if
:INCLUDE-CHILDREN is non-nil."
  (with-slots (visible child-views) view
    (setf (slot-value view 'visible) visibility)
    (when include-children
      (loop for child in child-views do
           (view-set-visibility child visibility)))))

(defun view-invalidate (view)
  "Invalidates a given view and asks it to redraw itself."
  (post-event view (make-instance 'draw-event)))
