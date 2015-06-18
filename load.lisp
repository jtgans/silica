(in-package #:cl-user)

(let ((packages '("cl-evdev"
                  "cl-gypsum-client"
                  "cl-event-handler"
                  "silica")))
  (loop for package in packages do
       (push (truename (concatenate 'string "../" package)) asdf:*central-registry*)))

(ql:quickload "silica")
