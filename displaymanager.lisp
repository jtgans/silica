(in-package :silica)

(defclass display-manager (handler)
  ()
  (:documentation "The DISPLAY-MANAGER, like the WINDOW-MANAGER and
INPUT-MANAGER is a singleton class that ensures that the connection to a Gypsum
display is active and running."))

(defmethod print-object ((object display-manager) stream)
  (print-unreadable-object (object stream :type t)))

(defmethod start-handler ((handler display-manager))
  (handler-case (connect :tcp)
    (sb-bsd-sockets:socket-error (socket-error)
      (log:error "~A while attempting to connect to Gypsum. Will retry later."
                 socket-error))))

;; TODO: Figure out how to expose disconnection events via the event handler
;; system.
;(defmethod handle-event ((handler display-manager) (event lost-connection-event))
;  )

(defvar *display-manager* (make-instance 'display-manager :start-immediately nil))
