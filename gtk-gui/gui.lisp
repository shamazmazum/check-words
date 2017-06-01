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
                  :accessor stream-input-string)
   (eof-pending   :initform nil
                  :accessor stream-eof-pending)))

(defmacro thread-safely (&body body)
  `(progn
     (gdk:gdk-threads-enter)
     (unwind-protect (progn ,@body)
       (gdk:gdk-threads-leave))))

(defun show-error-window (condition)
  (let ((dialog (make-instance 'gtk-message-dialog
                               :message-type :error
                               :buttons :ok
                               :text "An error occured"
                               :secondary-text
                               (with-output-to-string (stream)
                                 (princ condition stream)))))
    (gobject:g-signal-connect dialog "response"
                              (lambda (dialog response-id)
                                (declare (ignore response-id))
                                (gtk-widget-destroy dialog)))
    (gtk-widget-show dialog)))

(defun choose-dictionary ()
  (let ((dialog (gtk-file-chooser-dialog-new "Choose dictionary"
                                             nil :open
                                             "gtk-cancel" :cancel
                                             "gtk-open" :accept)))
    (prog1
        (if (eql (gtk-dialog-run dialog) :accept)
            (gtk-file-chooser-get-filename dialog))
      (gtk-widget-destroy dialog))))

(defun start-checker (filename output-buffer)
  (let ((stream (make-instance 'checker-stream :output-buffer output-buffer)))
    (make-instance 'checker
                   :stream stream
                   :thread
                   (handler-bind
                       ((esrap:esrap-parse-error
                         (lambda (c)
                           (show-error-window c)
                           (invoke-restart 'continue-with-parsed)))
                        #+sbcl
                        (sb-int:stream-decoding-error
                         (lambda (c)
                           (show-error-window c)
                           (invoke-restart 'sb-int:force-end-of-file))))
                     (check-dictionary filename
                                       :stream stream
                                       :threaded t)))))

(defun set-checker (checker)
  (if *checker*
      (let ((thread (checker-thread *checker*))
            (stream (checker-stream *checker*)))
        (when (thread-alive-p thread)
          (send-eof stream)
          (join-thread thread))))
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
    (condition-wait (stream-condvar stream)
                    (stream-lock stream))
    (if (stream-eof-pending stream)
        (error 'end-of-file :stream stream))
    (stream-input-string stream)))

(defgeneric set-input-string (stream string))
(defgeneric send-eof (stream))

(defmethod set-input-string ((stream checker-stream) string)
  (with-lock-held ((stream-lock stream))
    (setf (stream-input-string stream) string)
    (condition-notify (stream-condvar stream))))

(defmethod send-eof ((stream checker-stream))
  (with-lock-held ((stream-lock stream))
    (setf (stream-eof-pending stream) t)
    (condition-notify (stream-condvar stream))))

(defun run ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Check words"
                                 :default-width 250
                                 :default-height 70))
          (button-choose (make-instance 'gtk-file-chooser-button
                                        :label "Choose dictionary"
                                        :action :open))
          (button-next (make-instance 'gtk-button :label "Next"))
          (button-clear (make-instance 'gtk-button :label "Clear log"))
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
      (gobject:g-signal-connect button-choose "file-set"
                                (lambda (widget)
                                  (let ((filename (gtk-file-chooser-get-filename widget)))
                                    (when filename
                                      (set-checker (start-checker filename
                                                                  (gtk-text-view-buffer log-area)))))))
      (gobject:g-signal-connect button-next "clicked"
                                (lambda (widget)
                                  (declare (ignore widget))
                                  (when *checker*
                                    (set-input-string (checker-stream *checker*)
                                                      (gtk-entry-buffer-text
                                                       (gtk-entry-buffer answer-entry)))
                                    (gtk-window-set-focus window answer-entry))))

      (gobject:g-signal-connect button-clear "clicked"
                                (lambda (widget)
                                  (declare (ignore widget))
                                  (gtk-text-buffer-set-text
                                   (gtk-text-view-buffer log-area) "")))

      (gobject:g-signal-connect log-area "size-allocate"
                                (lambda (widget rectangle)
                                  (declare (ignore rectangle widget))
                                  (let ((adjustment (gtk-scrolled-window-vadjustment scrolled-window)))
                                    (setf (gtk-adjustment-value adjustment)
                                          (- (gtk-adjustment-upper adjustment)
                                             (gtk-adjustment-page-size adjustment))))))

      (gtk-container-add scrolled-window log-area)
      (gtk-box-pack-start box button-choose :expand nil :padding 2)
      (gtk-box-pack-start box label :expand nil :padding 2)
      (gtk-box-pack-start box answer-entry :expand nil :padding 2)
      (gtk-box-pack-start box button-next :expand nil)
      (gtk-box-pack-start box button-clear :expand nil :padding 2)
      (gtk-box-pack-start box scrolled-window)

      (gtk-container-add window box)
      (gtk-widget-show-all window))))
