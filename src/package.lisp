(defpackage check-words
  (:use #:cl #:esrap)
  (:export #:check-dictionary
           #:check-translation
           #:read-dictionary
           #:permute-translations
           #:*key-order*
           #:*io-stream*
           #:*standard-io*
           #:continue-with-parsed))
