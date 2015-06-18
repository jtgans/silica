(in-package :silica)

(defvar *event-loop-target* nil
  "The target for delivery of events. Must be a HANDLER instance.")

(defvar *event-device* "/dev/input/event"
  "The device file to read input events from.")

(defun read-event-loop ()
  "A loop that continually calls READ-EVENT to read evdev events from the device
specified in *EVENT-DEVIcE* and calls POST-EVENT on the HANDLER instance stored
in *EVENT-LOOP-TARGET*.

This is expected to be run from PEXEc.

NOTE: This is really naive -- we can't change devices mid-stream, and there's no
way to control the input mechanisms. This should probably be a handler with an
:around method."
  (with-binary-file (stream *event-device* :direction :input)
    (unwind-protect
       (loop do
          (let ((input-event (read-event stream)))
            (when (handler-alive-p *event-loop-target*)
              (post-event *event-loop-target* input-event)))))))
