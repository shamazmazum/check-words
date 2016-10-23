(in-package :check-words)

(defparameter *standard-io* (make-two-way-stream *standard-input* *standard-output*))
(defvar *io-stream* *standard-io*
  "check-words I/O stream")

(defun key-first (cons which)
  "Key-first key/value order"
  (declare (type (member :key :value) which))
  (case which
    (:key (car cons))
    (t (cdr cons))))

(defun key-last (cons which)
  "Key last key/value order"
  (declare (type (member :key :value) which))
  (case which
    (:key (cdr cons))
    (t (car cons))))

(defvar *get-key-val* #'key-last
  "Current key/value order")

(defun permute (list &optional res)
  (if (null list) res
      (let* ((idx (random (length list)))
             (item (nth idx list)))
        (permute (remove item list) (cons item res)))))

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

(defrule many-words (and group-begin
                         (* (and single-word group-separator))
                         single-word
                         group-end)
  (:destructure (brk1 list last-pair brk2)
                (declare (ignore brk1 brk2))
                (append
                 (mapcar #'caar list)
                 last-pair)))

(defrule comment (and #\# (* character))
  (:constant nil))

(defrule main-rule (or single-word many-words comment))

(defun check-group (group)
  "Question user for pairs group"
  (dolist (pair group)
    (format *io-stream* "~a? " (funcall *get-key-val* pair :key))
    (force-output *io-stream*)
    (apply #'format *io-stream*
           (let ((answer (read-line *io-stream*))
                 (correct-answer (funcall *get-key-val* pair :value)))
             (if (string= answer correct-answer)
                 '("Correct!~%") `("Wrong! Correct answer: ~a ~%" ,correct-answer))))))

(defun check-dictionary (filename &key (stream *io-stream*) (key-order :key-last))
  "Question user using a dictionary. STREAM is a used I/O stream and KEY-ORDER may be
:KEY-LAST or :KEY-FIRST"
  (let ((groups
         (with-open-file (in filename)
           (loop
              with groups = nil
              for line = (read-line in nil)
              while line
              do
                (handler-case
                    (let ((group (parse 'main-rule line)))
                      (if group (push group groups)))
                  (esrap-parse-error ()
                    (format *error-output* "Cannot parse line: ~a~%" line)))
              finally (return groups)))))
    (let ((*io-stream* stream)
          (*get-key-val*
           (case key-order
             (:key-last #'key-last)
             (t #'key-first))))
      (mapc #'check-group (permute groups))))
  t)
