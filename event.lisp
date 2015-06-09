(in-package :silica)

(defclass draw-event (event)
  ()
  (:documentation "An event that represents a redraw request."))

(defclass assign-event (event)
  ((frame :initarg :frame
          :type frame
          :reader assign-event-frame))
  (:documentation "An event that represents a frame assignment request."))

(defclass keyboard-event (event)
  ((code :initarg :code
         :type integer
         :reader keyboard-event-code)
   (name :initarg :name
         :type string
         :reader keyboard-event-name)
   (glyph :initarg :glyph
          :type character
          :reader keyboard-event-character)
   (state :initarg :state
          :type symbol
          :reader keyboard-event-state))
  (:documentation "An event that represents a keyboard input event."))
