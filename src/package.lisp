(defpackage check-words
  (:use #:cl #:esrap)
  (:export #:check-dictionary
           #:check-group
           #:read-groups
           #:permute-groups
           #:*key-order*
           #:*io-stream*
           #:*standard-io*))
