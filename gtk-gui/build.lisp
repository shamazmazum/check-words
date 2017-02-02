(require 'asdf)
(asdf:load-system :check-words-gtk)

#-sbcl (error "Not implemented")

(defun save-me ()
  (sb-ext:save-lisp-and-die "check-words-gtk"
                            :executable t :toplevel
                            (lambda ()
                              (setq *random-state* (make-random-state t))
                              (check-words-gtk:run)
                              (gtk:join-gtk-main))))
(save-me)
