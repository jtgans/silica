(in-package :silica)

(defvar *gap-width* 3
  "The size of the frame gaps in pixels.")

(defclass frame (view)
  ((direction    :initform nil :accessor frame-division :type symbol)
   (ratio        :initform 1/2 :accessor frame-ratio    :type ratio)
   (first-child  :initform nil :accessor frame-first    :initarg first  :type (or frame null))
   (second-child :initform nil :accessor frame-second   :initarg second :type (or frame null)))
  (:documentation "A frame is a VIEW that represents a tree of other VIEWs and
is used to manage application windows on screen."))

(defmethod print-object ((object view) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (direction ratio first-child second-child) object
      (format stream ":DIRECTION ~a :RATIO ~(~A~) :FIRST-CHILD ~A :SECOND_CHILD ~A"
              direction ratio first-child second-child))))

(defun frame-leaf-p (frame)
  "Determines if the given FRAME is a leaf node."
  (and (null (frame-first-child frame))
       (null (frame-second-child frame))))

(defun split-frame-leaf (frame direction &key (ratio 1/2) (gutter 1))
  "Splits a FRAME in two in the DIRECTION (:horizontal or :vertical),
creating two additional children with the given split RATIO."
  (unless (frame-leaf-p frame) (error "FRAME must be a leaf node."))
  (with-slots (rect first-child second-child parent-view child-views) frame
    (multiple-value-bind (first second)
        (subdivide-rect rect direction :ratio ratio :gutter gutter)
      ;; Set the parent's children to the new frames
      (setf first-child first
            second-child second)

      ;; Set the split direction and ratio for the parent
      (setf (frame-direction frame) direction
            (frame-ratio frame) ratio)

      ;; Move the child views to the new child
      (setf (view-children frame) nil
            (view-children first) child-views)

      ;; Set the parent on the new frames
      (post-event first-child (make-instance 'assign-event :frame frame))
      (post-event second-child (make-instance 'assign-event :frame frame))

      ;; Send the assign event to the children
      (loop for child in (view-children first) do
           (post-event child (make-instance 'assign-event :frame first))))))
