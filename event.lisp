(in-package :silica)

(defclass draw-event (event)
  ()
  (:documentation "An event that represents a draw request."))

(defclass assign-event (event)
  ((frame :initarg :frame
          :type frame
          :reader assign-event-frame))
  (:documentation "An event that represents a frame assignment request."))

(defclass layout-event (event)
  ((rect :initarg :rect
         :type rect
         :reader layout-event-rect))
  (:documentation "Represents a request to re-layout the contents of a VIEW."))
