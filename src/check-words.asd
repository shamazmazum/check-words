(defsystem :check-words
  :description "Check word translations"
  :serial t
  :version "1.0"
  :components ((:file "package")
               (:file "check-words"))
  :perform (test-op (op system)
                    (declare (ignore op system))
                    (asdf:load-system :check-words-test)
                    (funcall
                     (intern "RUN-TESTS" (find-package "CHECK-WORDS-TEST"))))
  :depends-on (:esrap))
