(in-package :check-words)

(defparameter *standard-io* (make-two-way-stream *standard-input* *standard-output*))
(defvar *io-stream* *standard-io*
  "check-words I/O stream")

(defvar *key-order* :last
  "Controls the key order in pair. May be :LAST or :FIRST")

(defun permute-groups (list)
  "Permute groups list"
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
(defrule group-separator #\;)
(defrule group-begin #\{)
(defrule group-end #\})

(defrule single-word (and word translation-separator word)
  (:lambda (list)
    (list
     (cons (first list)
           (third list)))))

(defrule annotation word) ; Just an alias

(defrule many-words (and group-begin
                         (? (and annotation group-separator))
                         (* (and single-word group-separator))
                         single-word
                         group-end)
  (:destructure (brk1 annotation list last-pair brk2)
                (declare (ignore brk1 brk2))
                (let ((pairs
                       (append
                        (mapcar #'caar list)
                        last-pair)))
                  (if annotation (cons (car annotation) pairs) pairs))))

(defrule comment (and #\# (* character))
  (:constant nil))

(defrule main-rule (or single-word many-words comment))

(defun read-groups (stream)
  "Read word pairs from stream"
  (loop
     with groups = nil
     for line = (read-line stream nil)
     while line
     do
       (handler-case
           (let ((group (parse 'main-rule line)))
             (if group (push group groups)))
         (esrap-parse-error ()
           (format *error-output* "Cannot parse line: ~a~%" line)))
     finally (return groups)))

(defun maybe-print-annotation (group)
  "Print a group annotation if any. Return the group without annotation"
  (let ((annotation (car group)))
  (cond
    ((atom annotation)
     (format *io-stream* "~a.~%" annotation)
     (cdr group))
    (t group))))

(defun check-group (group)
  "Question user for pairs group"
  (declare (type (member :first :last) *key-order*))
  (flet ((check-pair (pair)
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
                  nil))))))
    (mapcar #'check-pair (maybe-print-annotation group))))

(defun check-dictionary (filename &key (stream *io-stream*) (key-order *key-order*))
  "Question user using a dictionary. STREAM is a used I/O stream and KEY-ORDER may be
:LAST or :FIRST"
  (let ((groups
         (with-open-file (in filename)
           (read-groups in))))
    (let ((*io-stream* stream)
          (*key-order* key-order))
      (mapc #'check-group (permute-groups groups))))
  t)
