(in-package :silica)

(defclass display-manager (handler)
  ()
  (:documentation "The DISPLAY-MANAGER, like the WINDOW-MANAGER and
INPUT-MANAGER is a singleton class that ensures that the connection to a Gypsum
display is active and running."))

(defmethod print-object ((object display-manager) stream)
  (print-unreadable-object (object stream :type t)))

(defmethod start-handler ((handler display-manager))
  (connect :tcp))

(defmethod handle-event ((handler display-manager) (event lost-connection-event))
  (with-mutex (cl-gypsum-client:*write-lock*)
    ))

(defvar *display-manager* (make-instance 'display-manager :start-immediately nil))
