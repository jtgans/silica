(in-package :silica)

(defvar *screen-width* 640
  "The screen width in pixels.")

(defvar *screen-height* 360
  "The screen height in pixels.")

(defclass window-manager (handler)
  (:documentation "An application is simply a handler."))

(defvar *window-manager*
  (make-instance 'application
                 :name "Window Manager"))
