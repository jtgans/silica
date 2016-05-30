(in-package :silica)

(defclass input-manager (handler)
  ((event-device
    :initform "/dev/input/event17"
    :type string
    :initarg :device
    :documentation
    "The name of the evdev device to read events from.")
   (target
    :type handler
    :initarg :target
    :documentation
    "The root of the VIEW heirarchy to inject events via."))
  (:documentation "The main input handler routine for the entirety of silica.
This is the root of the input event graph."))

(defmethod handler-runloop ((handler input-manager))
  (with-slots (event-device target) handler
    (with-evdev-device (event event-device)
      (post-event target event))))

(defvar *input-manager*
  (make-instance 'input-manager :start-immediately nil :target *window-manager*)
  "The singleton instance of the input manager.")
