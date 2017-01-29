(require 'asdf)
(require 'sb-posix)
(asdf:load-system :check-words)

#-sbcl (error "Not implemented")

(defun get-args ()
  (cdr sb-ext:*posix-argv*))

(defun usage ()
  (format t "Usage: check-words <dictionary>~%")
  (exit :code 1))

(defun toplevel ()
  (let ((args (get-args)))
    (if (/= (length args) 1)
        (usage))
    (handler-case
        (let ((*random-state* (make-random-state t)))
          (check-words:check-dictionary (car args)))
      ((or sb-sys:interactive-interrupt end-of-file) ()
        (format t "Quiting on user demand~%")
        (exit :code 1))))
  (exit))

(defun save-me ()
  (sb-ext:save-lisp-and-die "check-words"
                            :executable t :toplevel #'toplevel))
(save-me)
