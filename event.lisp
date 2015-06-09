(in-package :silica)

(defclass event ()
  ()
  (:documentation "An event."))

(defclass ping-event (event)
  ()
  (:documentation "An event used to ping a handler for aliveness."))

(defclass quit-event (event)
  ()
  (:documentation "An event used to ask a handler to stop running."))

(defclass draw-event (event)
  ()
  (:documentation "An event that represents a draw request."))

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
