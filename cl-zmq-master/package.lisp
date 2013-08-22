;; Copyright (c) 2009, 2010 Vitaly Mayatskikh <v.mayatskih@gmail.com>
;;
;; This file is part of CL-ZMQ.
;;
;; Vitaly Mayatskikh grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(defpackage #:zeromq
  (:nicknames :zmq)
  (:use :cl :cffi)
  (:shadow #:close #:identity #:push)
  (:export
   ;; constants
   #:+affinity+
   #:+backlog+
   #:+dealer+
   #:+delimiter+
   #:+downstream+
   #:+efsm+
   #:+emthread+
   #:+enocompatproto+
   #:+events+
   #:+fd+
   #:+forwarder+
   #:+hwm+
   #:+identity+
   #:+linger+
   #:+max-vsm-size+
   #:+mcast-loop+
   #:+msg-more+
   #:+msg-shared+
   #:+noblock+
   #:+pair+
   #:+pollerr+
   #:+pollin+
   #:+pollout+
   #:+pub+
   #:+pull+
   #:+push+
   #:+queue+
   #:+rate+
   #:+rcvbuf+
   #:+rcvmore+
   #:+reconnect-ivl+
   #:+reconnect-ivl-max+
   #:+recovery-ivl+
   #:+recovery-ivl-msec+
   #:+rep+
   #:+req+
   #:+router+
   #:+sndbuf+
   #:+sndmore+
   #:+streamer+
   #:+sub+
   #:+subscribe+
   #:+swap+
   #:+type+
   #:+unsubscribe+
   #:+upstream+
   #:+vsm+
   #:+xrep+
   #:+xreq+

   ;; deprecated constants
   #:affinity
   #:delimiter
   #:downstream
   #:efsm
   #:emthread
   #:enocompatproto
   #:forwarder
   #:hausnumero
   #:hwm
   #:identity
   #:max-vsm-size
   #:mcast-loop
   #:msg-shared
   #:msg-tbc
   #:noblock
   #:pair
   #:poll
   #:pollerr
   #:pollin
   #:pollout
   #:pub
   #:pull
   #:push
   #:queue
   #:rate
   #:raw
   #:rcvmore
   #:recovery-ivl
   #:rep
   #:req
   #:sndmore
   #:streamer
   #:sub
   #:subscribe
   #:swap
   #:unsubscribe
   #:upstream
   #:vsm
   #:xrep
   #:xreq

   #:events

   ;; structures
   #:msg
   #:pollitem

   ;; functions
   #:bind
   #:close
   #:connect
   #:device
   #:errno
   #:getsockopt
   #:init
   #:msg-close
   #:msg-copy
   #:msg-data-as-array
   #:msg-data-as-is
   #:msg-data-as-string
   #:msg-init
   #:msg-init-data
   #:msg-init-size
   #:msg-move
   #:msg-raw
   #:msg-size
   #:msg-type
   #:pollitem-events
   #:pollitem-fd
   #:pollitem-raw
   #:pollitem-revents
   #:pollitem-socket
   #:recv
   #:send
   #:setsockopt
   #:socket
   #:strerror
   #:term
   #:version

   ;; macros
   #:with-context
   #:with-polls
   #:with-socket

   ;; conditions
   #:error-again))

(in-package :zeromq)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-foreign-library zeromq
    (:darwin (:or "libzmq.0.dylib" "libzmq.dylib"))
    (:unix (:or "libzmq.so.0.0.0" "libzmq.so"))
    (:windows "libzmq.dll")
    (t "libzmq")))

(use-foreign-library zeromq)
