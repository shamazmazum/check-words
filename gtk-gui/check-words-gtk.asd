(defsystem :check-words-gtk
  :description "Check word translations (GTK GUI)"
  :serial t
  :version "1.0"
  :components ((:file "package")
               (:file "gui"))
  :depends-on (:check-words
               :cl-cffi-gtk
               :bordeaux-threads
               :trivial-gray-streams))
