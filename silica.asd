;;;; silica.asd

(asdf:defsystem #:silica
  :description "The Core of the Navi project"
  :author "June Tate-Gans <june@theonelab.com>"
  :license "Simplified BSD License"
  :serial t
  :depends (:cl-gypsum-client :cl-evdev)
  :components ((:file "package")))
