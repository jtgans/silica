(in-package :silica)

(defvar *screen-width* 640
  "The screen width in pixels.")

(defvar *screen-height* 360
  "The screen height in pixels.")

(defvar *current-view* nil
  "The currently focused VIEW instance.")

(defclass window-manager (view)
  ((frame-tree :initform nil
               :accessor frame-tree
               :type frame))
  (:documentation "A WINDOW-MANAGER instance manages VIEWs inside of FRAMEs. It
handles calling the DRAW events, controls VIEW focus, and parcels out the
KEYBOARD-EVENT events to the currently focused VIEW.

This class is effectively a singleton -- only one should ever exist, and it is
accessible to all other threads through the special variable
*WINDOW-MANAGER*."))

(defmethod view-draw (view window-manager)
  "Draws the window manager and all of its frames to the screen.")

(defmethod print-object ((object window-manager) stream)
  (print-unreadable-object (object stream :type t)))

(defvar *window-manager*
  (make-instance 'window-manager :start-immediately nil)
  "The singleton instance of the window manager.")
