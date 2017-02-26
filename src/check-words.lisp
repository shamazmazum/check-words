(in-package :check-words)

(defparameter *standard-io* (make-two-way-stream *standard-input* *standard-output*))
(defvar *io-stream* *standard-io*
  "check-words I/O stream")

(defvar *key-order* :last
  "Controls the key order in pair. May be :LAST or :FIRST")

(define-condition check-words-parse-error (error)
  ())
(define-condition check-words-brace-balance (check-words-parse-error)
  ())

(let ((brace-count 0))
  (defun translate-brace (brace)
    (cond
      ((string= brace "{")
       (if (> brace-count 0)
           (error 'check-words-brace-balance))
       (incf brace-count)
       :group-start)
      ((string= brace "}")
       (if (< brace-count 1)
           (error 'check-words-brace-balance))
       (decf brace-count)
       :group-end))))

(defun permute-translations (list)
  "Permute a list"
  (labels ((do-permute (list res)
             (if (null list) res
                 (let* ((idx (random (length list)))
                        (item (nth idx list)))
                   (do-permute (remove item list) (cons item res))))))
    (do-permute list nil)))

(defun allowed (char)
  (not (member char '(#\{ #\} #\; #\= #\#) :test #'char=)))

(defrule translation-separator (+ #\=))
(defrule word (+ (allowed character))
  (:lambda (list)
    (string-trim '(#\Space #\Tab) (text list))))
(defrule pair-separator #\;)

(defrule group-begin #\{
    (:function translate-brace))
(defrule group-end #\}
  (:function translate-brace))

(defrule comment (and #\# (* character))
  (:constant nil))

(defrule single-pair (and word translation-separator word)
  (:lambda (list)
    (cons (first list)
          (third list))))

(defrule annotation (and word pair-separator)
  (:function first))

(defrule translation-pairs (and single-pair (* (and pair-separator single-pair)))
  (:destructure (first-pair rest-pairs)
                (cons first-pair
                      (mapcar #'second rest-pairs))))

(defrule main-rule (or comment (and (? (and group-begin (? annotation)))
                                    translation-pairs (? group-end)))
  (:lambda (list)
    (if list
        (destructuring-bind (group-marker1 pairs group-marker2) list
          (cons group-marker1 (cons group-marker2 pairs))))))

(defclass translation () ())
(defgeneric check-translation (translation)
  (:documentation "Question user for translation"))

(defclass single-translation (translation)
  ((translation :initarg :translation
                :accessor translation)))

(defclass translation-group (translation)
  ((annotation :initarg :annotation
               :initform nil
               :reader translation-annotation)
   (translations :initarg :translations
                 :initform nil
                 :accessor translations)))

(defmethod print-object ((translation translation-group) stream)
  (let ((translations (translations translation)))
    (pprint-logical-block (stream translations :prefix "#<translation-group " :suffix ">")
      (format stream "~s: "(translation-annotation translation))
      (pprint-newline :fill stream)
      (loop for pair = (pprint-pop) while t do
           (format stream "~s=~s; " (car pair) (cdr pair))
           (pprint-newline :fill stream)
           (pprint-exit-if-list-exhausted)))))

(defmethod print-object ((translation single-translation) stream)
  (let ((translation (translation translation)))
    (format stream "#<single-translation ~s=~s>"
            (car translation) (cdr translation))))

(defun collect-groups (list)
  "Stage2 parsing"
  (let (current-group)
    (flet ((do-collect (acc list)
             (destructuring-bind (group-start group-end &rest pairs) list
               (if group-start (setq current-group
                                     (make-instance 'translation-group
                                                    :annotation (second group-start))))
               (if current-group
                   (setf (translations current-group)
                         (append (translations current-group) pairs)))
               (cond
                 (group-end
                  (prog1 (cons current-group acc)
                    (setq current-group nil)))
                 (t
                  (if current-group acc
                      (append acc (mapcar (lambda (pair) (make-instance 'single-translation
                                                                        :translation pair))
                                          pairs))))))))
      (reduce #'do-collect list :initial-value nil))))

(defun read-dictionary (stream)
  "Read word pairs from file"
  (loop
     with groups = nil
     for line = (read-line stream nil)
     while line
     do
       (handler-case
           (let ((group (parse 'main-rule line)))
             (if group (push group groups)))
         ((or esrap-parse-error check-words-parse-error) ()
           (format *error-output* "Cannot parse line: ~a~%" line)))
     finally (return (collect-groups (reverse groups)))))

(defun check-pair (pair)
  (let ((get-key   (if (eq :first *key-order*) #'car #'cdr))
        (get-value (if (eq :first *key-order*) #'cdr #'car)))
    (format *io-stream* "~a? " (funcall get-key pair))
    (force-output *io-stream*)
    (let ((answer (read-line *io-stream*))
          (correct-answer (funcall get-value pair)))
      (cond
        ((string= answer correct-answer)
         (format *io-stream* "Correct!~%")
         t)
        (t
         (format *io-stream* "Wrong! Correct answer: ~a~%" correct-answer)
         nil)))))

(defmethod check-translation ((translation single-translation))
  (check-pair (translation translation)))

(defmethod check-translation ((translation translation-group))
  (let ((annotation (translation-annotation translation)))
    (if annotation
        (format *io-stream* "~a;~%" annotation)))
  (mapcar #'check-pair (translations translation)))

(defun check-dictionary (filename &key (stream *io-stream*) (key-order *key-order*) threaded)
  "Question user using a dictionary. STREAM is a used I/O stream and KEY-ORDER may be
:LAST or :FIRST. THREADED can be used to run checker in a separate thread (may be useful for GUI)"
  (let ((translations
         (with-open-file (stream filename)
           (read-dictionary stream))))
    (flet ((run% ()
             (let ((*io-stream* stream)
                   (*key-order* key-order))
               (loop for translation in (permute-translations translations) do
                    (handler-case (check-translation translation)
                      (end-of-file () (loop-finish)))
                    finally (return nil)))))
      (if threaded
          (bordeaux-threads:make-thread #'run% :name "Checker thread")
          (run%)))))
