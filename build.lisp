(in-package :cl-user)

(load "load.lisp")

(sb-ext:save-lisp-and-die "../out/silica" :toplevel
                          (lambda ()
                            (sb-posix:putenv (format nil "SBCL_HOME=~A" #.(sb-ext:posix-getenv "SBCL_HOME")))
                            (silica:start-silica)
                            0)
                          :executable t)
