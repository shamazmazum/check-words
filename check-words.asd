(defsystem :check-words
  :description "Check word translations"
  :serial t
  :version "1.0"
  :components ((:file "package")
               (:file "check-words"))
  :depends-on (:cl-factoradic
               :esrap))
