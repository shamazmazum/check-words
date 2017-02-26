(in-package :check-words-test)

(def-suite dict-parsing :description "Check dictionary parsing")
(def-suite group-checking :description "Check group checker itself")

(defun run-tests ()
  "Run all tests"
  (explain! (run 'dict-parsing))
  (explain! (run 'group-checking)))

(in-suite dict-parsing)

(defgeneric translation= (trans1 trans2))
(defmethod translation= ((trans1 check-words::single-translation)
                         (trans2 check-words::single-translation))
  (equalp (check-words::translation trans1)
          (check-words::translation trans2)))

(defmethod translation= ((trans1 check-words::translation-group)
                         (trans2 check-words::translation-group))
  (and
   (equalp (check-words::translations trans1)
           (check-words::translations trans2))
   (string= (check-words::translation-annotation trans1)
            (check-words::translation-annotation trans2))))

(defmethod translation= ((trans1 t) (trans2 t)) nil)

(test comment-parsing
  (let ((nothing
         (with-input-from-string (stream "# This is a comment~%")
           (read-dictionary stream))))
    (is (null nothing))))

(test single-translation-parsing
  ;; Test UTF-8 strings
  (let* ((expected-pair '("la quantità" . "-количество-"))
         (expected-translation (make-instance 'check-words::single-translation
                                              :translation expected-pair))
         (translation
          (with-input-from-string (stream (format nil "~a == ~a~%"
                                                  (car expected-pair)
                                                  (cdr expected-pair)))
            (read-dictionary stream))))
    (is (translation= (car translation)
                      expected-translation))))

(test group-parsing
  (let* (;; Diese Schreibweise ist veraltert aber geeignet für den Test
         (expected-translation (make-instance 'check-words::translation-group
                                              :translations '(("das Schloß" . "a lock")
                                                              ("das Schloß" . "a castle"))))
         (translation
          (with-input-from-string (stream (format nil "{ das Schloß = a lock~% das Schloß = a castle }~%"))
            (read-dictionary stream))))
        (is (translation= (car translation)
                          expected-translation))))

(test annotated-group-parsing
  (let* ((expected-translation (make-instance 'check-words::translation-group
                                              :translations '(("sono" . "io")
                                                              ("sei" . "tu")
                                                              ("è" . "egli"))
                                              :annotation "essere (presente)"))
         (translation
          (with-input-from-string (stream (format nil "{essere (presente); sono = io; sei = tu~% è = egli}~%"))
            (read-dictionary stream))))
    (is (translation= (car translation)
                      expected-translation))))

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
  (let ((group (make-instance 'check-words::translation-group
                                              :translations '(("замок" . "a lock")
                                                              ("замок" . "a castle")))))

    (let ((*key-order* :last))
      (with-answer-stream (*io-stream* (format nil "~a~%~a~%" "замок" "замок"))
      (is (every #'identity (check-translation group)))))

    (let ((*key-order* :first))
      (with-answer-stream (*io-stream* (format nil "~a~%~a~%" "a lock" "a castle"))
        (is (every #'identity (check-translation group)))))))
