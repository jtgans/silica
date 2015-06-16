(in-package #:cl-user)

(let ((packages '("cl-evdev"
                  "cl-gypsum-client"
                  "cl-event-handler"
                  "silica")))
  (loop for package in packages do
       (push (truename (concatenate 'string "../" package)) asdf:*central-registry*)))

(ql:quickload "silica")

(sb-ext:save-lisp-and-die "../out/silica" :toplevel
                          (lambda ()
                            (sb-posix:putenv (format nil "SBCL_HOME=~A" #.(sb-ext:posix-getenv "SBCL_HOME")))
                            (silica:silica)
                            0)
                          :executable t)
