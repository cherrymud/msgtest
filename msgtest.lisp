(defpackage :msgtest
            (:use :cl)
            (:use :msgtest.app-utils)
            (:export :-main))

(in-package :msgtest)

(defun -main (&optional args)
  (format t "Starting..~%")
  (setf nats:*debug* t)
  (defvar conn (nats:make-connection))
  (defvar sid (nats:subscribe conn "events" (lambda (msg) (format t "Received: ~A~%" msg))))

  (loop
    (format t "Sending message ~%")
    (nats:publish conn "events" "hi")
    (format t "Sent. Now sleeping for 1 second~%")
    (sleep 1)))
