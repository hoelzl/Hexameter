(in-package #:daktylos-impl)

(defvar *recv-tries* 100000
  "The number of times recv is called before aborting, if no data is received.")

(defvar *default-port* 55555
  "The default port for listening to requests.")

(defvar *default-encoding* :json
  "The default encoding for transmissions.")

(defvar *socket-cache* 0
  "The number of sockets that should be recycled; no caching if set to 0.")


;; TODO: initforms are only stand-ins
(define-class daktylos-context ()
  ((zeromq-context)
   (me :accessor me :type string :initform (format nil "localhost:~A" *default-port*)
       :documentation "The network name of the context.")
   (port :initform *default-port*
         :documentation "The network port used to listen for incoming messages.")
   (processor :type function :initform (lambda (type parameter author space) nil)
              :documentation "The callback function for incoming messages.")
   (resolver :type function :initform (lambda (component) component)
             :documentation "Function maps network addresses onto others.")
   (coder :type coder :initform (make-instance 'json-coder)
          :documentation "The coder class used for sent messages.")
   (respond-socket :documentation "The socket used to listen for requests.")
   (talk-sockets :documentation "A cache for sockets to talk to other components (not yet used).")))


(defmethod initialize-instance
    :after ((self daktylos-context)
            &key )
  (zmq:with-context (context 1)
    (setf (zeromq-context-of self) context)
    (zmq:with-socket (socket context zmq:+router+)
      (zmq:bind socket (format nil "tcp://*:~A" (port-of self)))
      (setf (respond-socket-of self) socket))))

(defmethod message ((self daktylos-context) msgtype recipient space parameter)
  (let* ((contents (plist-hash-table
                    (list :recipient recipient
                          :author (me self)
                          :type msgtype
                          :parameter parameter
                          :space space)))
         (body (encode (coder-of self) contents))
         (msg (format nil "~A~%~%~A" (name-of (coder-of self)) body)))
    (zmq:with-socket (socket (zeromq-context-of self) zmq:+dealer+)
      (zmq:connect socket (format nil "tcp://~A" recipient)) ;TODO: use resolver here
      (multisend socket "" msg))))

(defmethod respond ((self daktylos-context) tries) nil)

(defparameter *coder-names* '(("json" . json-coder)))

(defun find-coder-class (name)
  (cdr (assoc name *coder-names* :test #'string=)))

(defmethod find-coder-name ((class symbol))
  (car (rassoc class *coder-names* :test #'eql)))

(defmethod find-coder-name ((class class))
  (find-coder-name (class-name class)))

(define-abstract-class hexameter-coder ()
  ((name))
  (:documentation "Abstract class CODER.
Abstract superclass of all coders, i.e., classes that contain methods to serialize and
deserialize messages."))

(defmethod initialize-instance :after ((self hexameter-coder) &key)
  (unless (slot-boundp self 'name)
    (setf (name-of self) (find-coder-name (class-of self)))))

(defgeneric encode (coder data &optional stream)
  (:documentation
   "Encode DATA using CODER and write the result to stream.  If stream is NIL (the default),
return the result as string."))


;;; TODO: Specify how the decoding takes place and which message types have to be supported.
(defgeneric decode (coder message)
  (:documentation
   "Decode MESSAGE with CODER and return the resulting object."))

(define-class json-coder (hexameter-coder)
  ())

(defmethod encode ((coder json-coder) data &optional (stream nil))
  (encode-data-to-json data stream))

(defmethod decode ((coder json-coder) data)
  (decode-json data))

(defun send (socket data &optional flags)
  (let ((msg (make-instance 'zmq:msg :data data)))
    (zmq:send socket msg flags)))

(defun multisend (socket &rest frames)
  (dolist (frame (butlast frames))
    (send socket frame zmq:+sndmore+))
  (send socket (lastcar frames)))

(defun recv-string (socket &optional flags)
  (let ((msg (make-instance 'zmq:msg)))
    (zmq:recv socket msg flags)
    (zmq:msg-data-as-string msg)))

(defun multirecv (socket &optional flags)
  (let ((frames '()))
    (push (recv-string socket flags) frames)
    (while (= (zmq:getsockopt socket zmq:+rcvmore+) 1)
      (push (recv-string socket) frames))
    (nreverse frames)))
