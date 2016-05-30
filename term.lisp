(in-package :silica)

(defclass term ()
  ((rect :type rect
         :initarg :rect
         :documentation
         "The range of the screen that this view takes up.")
   (font-width :type integer
               :initarg :font-width
               :documentation
               "The width of a character cell in the current font, in pixels.")
   (font-height :type integer
                :initarg :font-height
                :documentation
                "The height of a character cell in the current font, in pixels.")
   (program :initform nil
            :type string
            :initarg :program
            :documentation
            "The program to start for the terminal. Defaults to the SHELL environment variable if nil.")

   (cursor-pos-x :type integer
                 :initform 0
                 :documentation
                 "The X position of the character cell cursor.")
   (cursor-pos-y :type integer
                 :initform 0
                 :documentation
                 "The Y position of the character cell cursor.")
   (process :initform nil
            :type object
            :documentation
            "The process structure returned by sb-ext:run-program.")))

(defgeneric start-application (term)
  (:documentation "Starts the given application."))

(defgeneric handle-process-status-change (term process)
  (:documentation "Handles a process status change."))

(defgeneric handle-evdev-input-event (term cl-evdev:input-event)
  (:documentation "Handles an evdev input event."))

(defgeneric term-putchar (term character)
  (:documentation "Renders the given character to the screen."))

(defmethod handle-evdev-input-event ((term term) (event cl-evdev:input-event))
  ;; TODO(jtgans): Do something with generic input events.
  )

(defmethod handle-evdev-input-event ((term term) (event cl-evdev:keyboard-event))
  (with-slots (process) term
    (let ((pty (sb-ext:process-pty process))
          (char (slot-value event 'cl-evdev::glyph)))
      (when (and (eq (slot-value event 'cl-evdev::state) :pressed)
                 char)
        (write-char char pty)
        (term-putchar term char)))))

(defmethod handle-process-status-change ((term term) process)
  ;; TODO(jtgans): handle the process status change here
  )

(defmethod start-application ((term term))
  "Starts the given terminal application."
  (with-slots (input-stream output-stream program process) term
    (let ((start-directory (or (sb-ext:posix-getenv "HOME")
                               "/")))
      (unless program
        (setf program (or (sb-ext:posix-getenv "SHELL")
                          "/bin/sh")))
      (setf process
            (sb-ext:run-program program ()
                                :wait nil
                                :pty t
                                :status-hook (lambda (process) (handle-process-status-change term process))
                                :directory start-directory))
      (chanl:pexec ()
        (loop (cl-evdev:with-evdev-device (event "/dev/input/event17")
                (handle-evdev-input-event term event))))

      (chanl:pexec ()
        (let ((pty (sb-ext:process-pty process)))
          (loop
             for char = (read-char pty)
             when char
             do (term-putchar term char)))))))

(defmethod cursor-pixel-x ((term term))
  "Returns the X pixel position of the character cell cursor."
  (with-slots (cursor-pos-x font-width rect) term
    (+ (* font-width cursor-pos-x) (rect-left rect))))

(defmethod cursor-pixel-y ((term term))
  "Returns the Y pixel position of the character cell cursor."
  (with-slots (cursor-pos-y font-height rect) term
    (+ (* font-height cursor-pos-y) (rect-top rect))))

(defmethod term-putchar ((term term) (char character))
  "Puts a character into the term window, raw. No character classes are
interpreted. The cursor position is updated."
  (with-slots (rect cursor-pos-x cursor-pos-y font-width font-height) term
    (let ((term-width (floor (/ (rect-width rect) font-width)))
          (term-height (floor (/ (rect-height rect) font-height)))
          (x (cursor-pixel-x term))
          (y (cursor-pixel-y term))
          (extent-start-x (rect-left rect))
          (extent-start-y (rect-top rect))
          (extent-end-x   (rect-right rect))
          (extent-end-y   (rect-bottom rect)))
      (with-gypsum-context
        (text :text (string char) :start `(,x . ,y) :color "white")
        (commit))
      (incf cursor-pos-x)
      (when (> cursor-pos-x term-width)
        (incf cursor-pos-y)
        (setf cursor-pos-x 0))
      (when (> cursor-pos-y term-height)
        (setf cursor-pos-y 0)
        (with-gypsum-context
          (box :start `(,extent-start-x . ,extent-start-y)
               :end   `(,extent-end-x . ,extent-end-y)
               :color "black"
               :filled t)))))
  (values))
