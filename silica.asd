;;;; silica.asd

(asdf:defsystem #:silica
  :description "The Core of the Navi project"
  :author "June Tate-Gans <june@theonelab.com>"
  :license "Simplified BSD License"
  :serial t
  :depends-on (#:cl-gypsum-client
               #:cl-evdev
               #:sb-events
               #:bordeaux-threads
               #:chanl)
  :components ((:file "package")
               (:file "silica")
               (:file "event")
               (:file "frame")
               (:file "handler")
               (:file "rect")
               (:file "view")
               (:file "window")
               (:file "windowmanager")))
