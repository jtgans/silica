;;;; silica.lisp

(in-package #:silica)

(defclass system-handler (handler)
  ()
  (:documentation "This handler represents the Silica system event handler. It's
used to bring up and shutdown the system. Effectively, all this handler does is
wait for a QUIT-EVENT and then terminates the system."))

(defmethod print-object ((object system-handler) stream)
  (print-unreadable-object (object stream :type t)))

(defmethod start-handler ((handler system-handler))
  "Starts the SYSTEM-HANDLER in a synchronous way to prevent exits from
occurring before we're ready."
  (with-slots (name event-channel event-task) handler
    (log:info "System handler starting, waiting for events.")
    (handler-runloop handler)))

(defvar *system-handler* (make-instance 'system-handler :start-immediately nil)
  "The one reference to the single SYSTEM-HANDLER instance in the system.")

(defun start-silica ()
  "Starts the system. Effectively just starts any of the handlers requested at
startup, connects to Gypsum, and starts the system event handler."
  (log:info "Silica v0.1~%")
  (pcall #'read-event-loop)
  (start-handler *display-manager*)
  (start-handler *system-handler*))

(export '(start-silica))
