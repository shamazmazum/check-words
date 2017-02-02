(in-package :check-words-gtk)

(defvar *checker* nil)

(defclass checker ()
  ((thread :initarg :thread
           :initform (error "Specify a thread")
           :reader checker-thread)
   (stream :initarg :stream
           :initform (error "Specify a stream")
           :reader checker-stream)))

(defclass checker-stream (fundamental-character-output-stream
                          fundamental-character-input-stream)
  ((lock    :initform (make-lock "Stream lock")
            :reader stream-lock)
   (condvar :initform (make-condition-variable)
            :reader stream-condvar)
   (output-buffer :initarg :output-buffer
                  :initform (error "Specify output buffer")
                  :reader stream-output-buffer)
   (input-string  :initform ""
                  :accessor stream-input-string)))

(defmacro thread-safely (&body body)
  `(progn
     (gdk:gdk-threads-enter)
     (unwind-protect (progn ,@body)
       (gdk:gdk-threads-leave))))

(defun start-checker (filename output-buffer)
  (let ((stream (make-instance 'checker-stream :output-buffer output-buffer))
        (groups
         (with-open-file (in filename)
           (permute-groups (read-groups in)))))
    (make-instance 'checker
                   :stream stream
                   :thread
                   (make-thread (lambda ()
                                  (let ((*io-stream* stream))
                                    (mapc #'check-group groups)))
                                :name "Checker thread"))))

(defun set-checker (checker)
  (if (and *checker* (thread-alive-p (checker-thread *checker*)))
      (destroy-thread (checker-thread *checker*)))
  (setq *checker* checker))

(defmethod stream-write-char ((stream checker-stream) char)
  (thread-safely
   (let* ((buffer (stream-output-buffer stream))
          (text (gtk-text-buffer-text buffer)))
     (gtk-text-buffer-set-text buffer
                               (concatenate 'string
                                            text
                                            (list char))))))

(defmethod stream-terpri ((stream checker-stream))
    (thread-safely
   (let* ((buffer (stream-output-buffer stream))
          (text (gtk-text-buffer-text buffer)))
     (gtk-text-buffer-set-text buffer
                               (concatenate 'string
                                            text
                                            (list #\NewLine))))))

(defmethod stream-read-line ((stream checker-stream))
  (with-lock-held ((stream-lock stream))
    (with-accessors ((input-string stream-input-string)) stream
      (loop while (= (length input-string) 0) do
           (condition-wait (stream-condvar stream)
                           (stream-lock stream)))
      (prog1
          input-string
        (setf input-string "")))))

(defgeneric set-input-string (stream string))
(defmethod set-input-string ((stream checker-stream) string)
  (with-lock-held ((stream-lock stream))
    (setf (stream-input-string stream) string)
    (condition-notify (stream-condvar stream))))

(defun choose-dictionary ()
  (let ((dialog (gtk-file-chooser-dialog-new "Choose dictionary"
                                             nil :open
                                             "gtk-cancel" :cancel
                                             "gtk-open" :accept)))
    (prog1
        (if (eql (gtk-dialog-run dialog) :accept)
            (gtk-file-chooser-get-filename dialog))
      (gtk-widget-destroy dialog))))

(defun run ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Check words"
                                 :default-width 250
                                 :default-height 70))
          (button1 (make-instance 'gtk-file-chooser-button
                                  :label "Choose dictionary"
                                  :action :open))
          (button2 (make-instance 'gtk-button :label "Next"))
          (label (make-instance 'gtk-label :label "Press Start to choose dictionary"))
          (answer-entry (make-instance 'gtk-entry))
          (log-area (make-instance 'gtk-text-view :editable nil))
          (box (make-instance 'gtk-box :orientation :vertical :spacing 6))
          (scrolled-window (make-instance 'gtk-scrolled-window)))

      (gobject:g-signal-connect window "destroy"
                                (lambda (widget)
                                  (declare (ignore widget))
                                  (set-checker nil)
                                  (leave-gtk-main)))
      (gobject:g-signal-connect button1 "file-set"
                                (lambda (widget)
                                  (let ((filename (gtk-file-chooser-get-filename widget)))
                                    (when filename
                                      (set-checker (start-checker filename
                                                                  (gtk-text-view-buffer log-area)))))))
      (gobject:g-signal-connect button2 "clicked"
                                (lambda (widget)
                                  (declare (ignore widget))
                                  (set-input-string (checker-stream *checker*)
                                                    (gtk-entry-buffer-text
                                                     (gtk-entry-buffer answer-entry)))
                                  (gtk-window-set-focus window answer-entry)))

      (gtk-container-add scrolled-window log-area)
      (gtk-box-pack-start box button1 :expand nil :padding 2)
      (gtk-box-pack-start box label :expand nil :padding 2)
      (gtk-box-pack-start box answer-entry :expand nil :padding 2)
      (gtk-box-pack-start box button2 :expand nil :padding 2)
      (gtk-box-pack-start box scrolled-window)

      (gtk-container-add window box)
      (gtk-widget-show-all window))))
