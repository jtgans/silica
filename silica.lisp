;;;; silica.lisp

(in-package #:silica)

(export 'silica)

(defvar *screen-width* 640
  "The width of the Gypsum screen.")

(defvar *screen-height* 360
  "The height of the Gypsum screen.")

(defun silica ()
  (format t "Silica v0.1~%"))
