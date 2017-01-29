(defsystem :check-words-test
  :name :checl-words-test
  :version "0.1"
  :author "Vasily Postnicov <shamaz.mazum at gmail dot com>"
  :components ((:file "package")
               (:file "test" :depends-on ("package")))
  :depends-on (:check-words :fiveam :trivial-gray-streams))
