;; Copyright (c) 2009, 2010 Vitaly Mayatskikh <v.mayatskih@gmail.com>
;;
;; This file is part of CL-ZMQ.
;;
;; Vitaly Mayatskikh grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(asdf:oos 'asdf:load-op :zeromq)

(defpackage :zeromq-test
  (:use :cl))

(in-package :zeromq-test)

(load "thr-parms")
(load "helpers")

(zmq::with-context (ctx 1)
  (zmq:with-socket (s ctx zmq:pub)
    (zmq:setsockopt s zmq:rate *rate*)
    (zmq:connect s *connect-address*)
    (let ((msg (make-instance 'zmq:msg)))
      (dotimes (i *message-count*)
        (zmq:msg-init-size msg *message-size*)
        (zmq:send s msg)
        (zmq:msg-close msg))
      (sleep 1))))

(tg:gc)
#+sbcl (sb-ext:quit)
#+clisp (ext:quit)
#+ccl (ccl:quit)
#+ecl (ext:quit)
