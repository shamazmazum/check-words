(require 'asdf)
(require 'sb-posix)
(asdf:load-system :check-words)

(defun get-args ()
  #+sbcl (cdr sb-ext:*posix-argv*)
  #-sbcl (progn
           (error "Not implemented")
           (exit :code 1)))

(defun usage ()
  (format t "Usage: check-words <dictionary>~%")
  (exit :code 1))

(defun toplevel ()
  (let ((args (get-args)))
    (if (/= (length args) 1)
        (usage))
    (words-translation:check-dictionary (car args)))
  (exit))

(defun save-me ()
  #+sbcl (sb-ext:save-lisp-and-die (sb-posix:getenv "CHECK_WORDS_NAME")
                                   :executable t :toplevel #'toplevel))
(save-me)
