(in-package #:daktylos-impl)

(defvar *recv-tries* 100000
  "The number of times recv is called before aborting, if no data is received.")

(defvar *default-port* 55555
  "The default port for listening to requests.")

(defvar *default-encoding* :json
  "The default encoding for transmissions.")

(defvar *socket-cache* 0
  "The number of sockets that should be recycled; no caching if set to 0.")

;;; TODO: Find a proper name for this class.
(define-class daktylos ()
  ((context :initform (required-argument :context))
   (self :initform (required-argument :self))
   (port :initform *default-port*)
   (processor)
   (resolver)
   (coder)
   (respond-socket)
   (talk-socket)))


(define-abstract-class coder ()
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

(define-class json-coder (coder)
  ())

(defmethod encode ((coder json-coder) data &optional (stream nil))
  (if stream
      (cl-json:encode-json data stream)
      (cl-json:encode-json-to-string data)))

(defmethod decode ((coder json-coder) (data string))
  (cl-json:decode-json-from-string data))

(defmethod decode ((coder json-coder) (data stream))
  (cl-json:decode-json data))
