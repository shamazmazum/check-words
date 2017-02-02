(defpackage check-words-gtk
  (:use #:cl #:check-words #:gtk
        #:bordeaux-threads #:trivial-gray-streams)
  (:export #:run))
