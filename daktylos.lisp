(in-package #:daktylos-impl)

(defvar *encoding* :iso-8859-1)

(defvar *recv-tries* 10000
  "The number of times recv is called before aborting, if no data is received.")

(defvar *default-port* 55555
  "The default port for listening to requests.")

(defvar *default-encoding* :json
  "The default encoding for transmissions.")

(defvar *socket-cache* 0
  "The number of sockets that should be recycled; no caching if set to 0.")

(defun init (&rest args)
  (apply 'make-instance 'daktylos-context args))

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
   (coder :type hexameter-coder :initform (make-instance 'json-coder)
          :documentation "The coder class used for sent messages.")
   (respond-socket :documentation "The socket used to listen for requests.")
   (talk-sockets :documentation "A cache for sockets to talk to other components (not yet used).")))


(defmethod initialize-instance
    :after ((self daktylos-context)
            &key )
  (let ((context (pzmq:ctx-new)))
    (setf (zeromq-context-of self) context)
    (let ((socket (pzmq:socket context :router)))
      (pzmq:bind socket (format nil "tcp://*:~A" (port-of self)))
      (setf (respond-socket-of self) socket))))

(defmethod couple ((self daktylos-context) processor)
  (setf (processor-of self) processor))

(defmethod message ((self daktylos-context) msgtype recipient space parameter)
  (let* ((contents (plist-hash-table
                    (list :recipient recipient
                          :author (me self)
                          :type msgtype
                          :parameter parameter
                          :space space)))
         (body (encode (coder-of self) contents))
         (msg (format nil "~A~%~%~A" (name-of (coder-of self)) body)))
    (pzmq:with-socket (socket (zeromq-context-of self)) :dealer
      (pzmq:connect socket (format nil "tcp://~A" recipient)) ;TODO: use resolver here
      (multisend socket "" msg))))

(defmethod respond ((self daktylos-context) &optional (tries *recv-tries*))
  (let ((frames '()))
    (if (= tries 0)
        (setf frames (multirecv (respond-socket-of self)))
        (let ((i 0))
          (while (and (not frames) (< i tries))
            (setf frames (multirecv (respond-socket-of self) :dontwait t)) ;TODO: this call throws an error from inside pzmq:recv-string
            (setf i (+ i 1)))))
    (if (cdr frames)
        (destructuring-bind (src del msg) frames
          (destructuring-bind (header body) (cl-ppcre:split "\\n\\n" msg)
            (let ((coder (make-instance (find-coder-class header))))
              (let ((mess (decode coder body)))
                ; (print (gethash "author" mess))
                (let ((resp (funcall (processor-of self)
                                     (gethash "type" mess)
                                     (gethash "author" mess)
                                     (gethash "space" mess)
                                     (gethash "parameter" mess))))
                  (if resp
                      (message self 
                               "ack"
                               (gethash "author" mess)
                               (gethash "space" mess)
                               resp)
                      t))
          ))))
        nil)))
        

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

(defun multisend (socket &rest frames)
  (dolist (frame (butlast frames))
    (pzmq:send socket frame :encoding *encoding* :sndmore t))
  (pzmq:send socket (lastcar frames) :encoding *encoding*))

(defun multirecv (socket &key dontwait)
  (let ((frames '()))
    (push (pzmq:recv-string socket :dontwait dontwait :encoding *encoding*) frames)
    (while (pzmq:getsockopt socket :rcvmore)
      (push (pzmq:recv-string socket :encoding *encoding*) frames))
    (nreverse frames)))
