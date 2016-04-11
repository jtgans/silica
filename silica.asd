;;;; silica.asd

(asdf:defsystem #:silica
  :description "The Core of the Navi project"
  :author "June Tate-Gans <june@theonelab.com>"
  :license "Simplified BSD License"
  :serial t
  :depends-on (#:cl-gypsum-client
               #:cl-evdev
               #:cl-event-handler
               #:binary-types
               #:alexandria
               #:chanl)
  :components ((:file "package")
               (:file "silica")
               (:file "event")
               (:file "rect")
               (:file "view")
               (:file "frame")
               (:file "window")
               (:file "windowmanager")
               (:file "displaymanager")
               (:file "input")))
