(in-package :check-words-test)

(def-suite dict-parsing :description "Check dictionary parsing")
(def-suite group-checking :description "Check group checker itself")

(defun run-tests ()
  "Run all tests"
  (explain! (run 'dict-parsing))
  (explain! (run 'group-checking)))

(in-suite dict-parsing)

(defun group-as-string (group)
  "Return a group string representation"
  (format nil "{ ~{~a = ~a~^; ~} }"
          (reduce (lambda (pair acc)
                    (cons (car pair)
                          (cons (cdr pair) acc)))
                  group
                  :initial-value nil
                  :from-end t)))

(defun annotated-group-as-string (group)
  "Return an annotated group string representation"
  (format nil "{~a; ~{~a = ~a~^; ~} }"
          (car group)
          (reduce (lambda (pair acc)
                    (cons (car pair)
                          (cons (cdr pair) acc)))
                  (cdr group)
                  :initial-value nil
                  :from-end t)))

(test comment-parsing
  (let ((nothing
         (with-input-from-string (stream "# This is a comment")
           (read-groups stream))))
    (is (null nothing))))

(test single-pair-parsing
  ;; Test UTF-8 strings
  (let* ((expected-pair '("la quantità" . "количество"))
         (pair
          (with-input-from-string (stream (format nil "~a == ~a"
                                                  (car expected-pair)
                                                  (cdr expected-pair)))
            (read-groups stream))))
    (is (equalp expected-pair (caar pair))))
  ;; Test non-alphanumeric characters in input
(let* ((expected-pair '("irgendwer" . "кто-то"))
         (pair
          (with-input-from-string (stream (format nil "~a == ~a"
                                                  (car expected-pair)
                                                  (cdr expected-pair)))
            (read-groups stream))))
    (is (equalp expected-pair (caar pair)))))

(test group-parsing
  (let* ((expected-group
          ;; Diese Schreibweise ist veraltert aber geeignet für den Test
          '(("das Schloß" . "a lock")
            ("das Schloß" . "a castle")))
         (group
          (with-input-from-string (stream (group-as-string expected-group))
            (read-groups stream))))
    (is (equalp expected-group (car group)))))

(test annotated-group-parsing
  (let* ((expected-group
          '("essere (presente)"
            ("sono" . "io")
            ("sei" . "tu")
            ("è" . "egli")))
         (group
          (with-input-from-string (stream (annotated-group-as-string expected-group))
            (read-groups stream))))
    (is (equalp expected-group (car group)))))

(in-suite group-checking)

(defclass null-output (fundamental-character-output-stream)
  ()
  (:documentation "Output to nowhere"))

(defmethod stream-write-char ((stream null-output) char)
  (declare (ignore stream))
  char)

(defmethod stream-terpri ((stream null-output))
  (declare (ignore stream))
  nil)

(defun make-answer-stream (answer)
  "Make a stream which ignores any output and get an ANSWER on input"
  (make-two-way-stream (make-string-input-stream answer)
                       (make-instance 'null-output)))

(defmacro with-answer-stream ((stream answer) &body body)
  `(let ((,stream (make-answer-stream ,answer)))
     (unwind-protect
          (progn ,@body)
       (close ,stream))))

(test group-checking
  (let ((group '(("замок" . "a lock")
                 ("замок" . "a castle")))
        (*key-order* :last))
    (with-answer-stream (*io-stream* (format nil "~a~%~a~%" "замок" "замок"))
      (is (every #'identity (check-group group)))))
  (let ((group '(("замок" . "a lock")
                 ("замок" . "a castle")))
        (*key-order* :first))
    (with-answer-stream (*io-stream* (format nil "~a~%~a~%" "a lock" "a castle"))
      (is (every #'identity (check-group group))))))
