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
         :documention "The network port used to listen for incoming messages.")
   (processor :type function :initform (lambda (type parameter author space) nil)
              :documentation "The callback function for incoming messages.")
   (resolver :type function :initform (lambda (component) component)
             :documentation "Function maps network addresses onto others.")
   (coder :type coder :initform :json
          :documentation "The coder class used for sent messages.")
   (respond-socket :documentation "The socket used to listen for requests.")
   (talk-sockets :documentation "A cache for sockets to talk to other components (not yet used).")))


(defmethod initialize-instance
    :after ((self daktylos-context)
            &key )
  (zmq:with-context (context 1)
    (setf (zeromq-context-of self) context)
    (zmq:with-socket (socket context zmq:router) ;ISSUE: ok, binding doesn't understand router/dealer, wtf
      (zmq:bind socket (format nil "tcp://*:~A" (port-of self)))
      (setf (respond-socket-of self) socket))))

(defmethod message ((self daktylos-context) msgtype recipient space parameter)
  (let ((msg (format nil "~A~%~%~A" (name-of (coder-of self)) (encode (coder-of self) '())))) ;TODO: analyze what data types we need to use and replace '()
    (zmq:with-socket (socket (context-of self) zmq:dealer)
      (zmq:connect socket (format nil "tcp://~A" recipient)) ;TODO: use resolver here
      ;(multisend socket "" msg)                              
)))

(defmethod respond ((self daktylos-context) tries) nil)


(define-abstract-class hexameter-coder ()
  ()
  (:documentation "Abstract class CODER.
Abstract superclass of all coders, i.e., classes that contain methods to serialize and
deserialize messages."))

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
  (if stream
      (cl-json:encode-json data stream)
      (cl-json:encode-json-to-string data)))

(defmethod decode ((coder json-coder) (data string))
  (cl-json:decode-json-from-string data))

(defmethod decode ((coder json-coder) (data stream))
  (cl-json:decode-json data))
