;;;; package.lisp

(defpackage #:silica
  (:use #:cl
        #:cl-event-handler
        #:cl-evdev
        #:cl-gypsum-client)
  (:documentation "The core of the Navi operating environment."))
