(in-package :silica)

(defclass handler ()
  ((name
    :initform nil
    :type string
    :initarg :name
    :documentation "The name of this handler.")
   (event-channel
    :initform (make-instance 'unbounded-channel)
    :initarg :event-channel
    :documentation "The channel to use for listening for inbound events."))
  (:documentation "A handler is an event handling object that spawns a thread to
dispatch events placed on its queue. Events are sent to the handler via the
`post-event' generic method, and are executed via the `handle-event' generic
method."))

;;;; Interface

(defgeneric post-event (handler event)
  (:documentation "Posts EVENT to HANDLER's queue."))
(defgeneric handle-event (handler event)
  (:documentation "Handles an EVENT on HANDLER's queue from HANDLER's
event-thread context. The default method does nothing."))
(defgeneric start-handler (handler)
  (:documentation "Starts HANDLER's event loop if it is not running already."))

;;;; Default methods and functions

(defmethod print-object ((object handler) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (name event-channel) object
      (format stream ":NAME ~a :EVENT-cHANNEL ~a"
              name event-channel))))

(defmethod post-event ((handler handler) (event event))
  (with-slots (event-channel) handler
    (send event-channel event)))

(defmethod handle-event ((handler handler) (event event))
  (log:warn "Unhandled event for ~a: ~a" handler event)
  (values))

(defmethod handle-event ((handler handler) (event ping-event))
  (log:info "PONG from ~a" handler)
  (values))

(defmethod handle-event ((handler handler) (event quit-event))
  (log:info "Quit received -- terminating thread.")
  (terminate-thread (slot-value handler 'event-thread))
  (values))

(defmethod initialize-instance :after ((handler handler) &key name)
  (with-slots (event-thread event-waitqueue queue-lock) handler
    (setf event-waitqueue (make-waitqueue :name (concatenate 'string name "-waitqueue"))
          queue-lock (make-mutex :name (concatenate 'string name "-queuemutex")))))

(defmethod start-handler ((handler handler))
  (with-slots (event-thread name) handler
    (when (or (null event-thread) (not (thread-alive-p event-thread))))
      (setf event-thread (make-thread #'(lambda () (handler-runloop handler))
                                      :name name))
      (log:info "Event loop started for handler ~a" handler)))

(defun handler-runloop (handler)
  "Handles each event on the event queue by popping the end of calling `handle-event' on each event
in the queue."
  (with-slots (event-channel) handler
    (loop
       (let ((event (recv event-channel)))
         (handle-event handler event)))))

(defmethod start-handler ((handler handler))
  (with-slots (name event-channel) handler
    (pcall #'(lambda () (handler-runloop handler)))
    (log:info "Event loop started for handler ~a" handler)))
