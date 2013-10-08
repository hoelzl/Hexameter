(in-package #:hexameter-impl)

(defvar *default-host* "localhost"
  "The default hostname to reach the component.")

(defvar *default-port* 55555
  "The default port for listening to requests.")

(defvar *default-space* 'behavior:verbose-memory-space
  "The default space used to react to messages.")

(defvar *default-spheres* (list 'behavior:flagging-sphere 'behavior:networking-sphere 'behavior:verbose-sphere)
  "The default spheres used to process messsages before passing them to the space.")

(defvar *default-coder* (make-instance 'medium:json-coder)
  "The en-/decoder used to serialize message data for transmission")

(defvar *recv-tries* 10000
  "The number of times recv is called before aborting, if no data is received.")


;;; Basic hexameter context
;;; =======================

(defun init (&rest args)
  (apply 'make-instance 'hexameter-context args))

(define-class hexameter-context ()
  ((me :accessor me :type string :initform (format nil "~A:~A" *default-host* *default-port*)
       :documentation "The network name of the context.")
   (medium :type daktylos-context
           :documentation "The context of the message transmission module.")
   (behavior :type spondeios-context
             :documentation "The context of the message processing module.")))

(defmethod initialize-instance
    :after ((self hexameter-context)
            &key (space *default-space*) (spheres *default-spheres*) (coder *default-coder*))
  (multiple-value-bind (name resolver)
      (cond ((typep (me self) 'string)
             (if (cl-ppcre:scan ":" (me self)) (values (me self) nil) (values (format nil "~A:~A" (me self) *default-port*) nil)))
            ((typep (me self) 'number)
             (values (format nil "~A:~A" *default-host* (me self)) nil))
            ((typep (me self) 'function)
             (values (funcall (me self) nil) (me self)))
            ((typep (me self) 'null)
             (values (format nil "~A:~A" *default-host* *default-port*)))
            (t
             (warn "Provided unsuitable value ~A for :me when initializing hexameter." (me self))
             (values (format nil "~A:~A" *default-host* *default-port*))))
    (setf (me self) name)
    (setf (behavior-of self) (behavior:init :space space :spheres spheres :me (me self)))
    (setf (medium-of self) (medium:init :coder coder :resolver resolver :me (me self))))
  (let ((behavior-success
          (behavior:couple (behavior-of self)
                           (lambda (msgtype recipient space parameter)
                             (medium:message (medium-of self) msgtype recipient space parameter))))
        (medium-success
          (medium:couple (medium-of self)
                         (lambda (msgtype author space parameter)
                           (behavior:process (behavior-of self) msgtype author space parameter)))))
    (values medium-success behavior-success)))

(defmethod term ((self hexameter-context))
  (values (medium:term (medium-of self)) (behavior:term (behavior-of self))))

(defmethod tell ((self hexameter-context) msgtype recipient space parameter)
  (behavior:act (behavior-of self) msgtype recipient space parameter))

(defmethod process ((self hexameter-context) msgtype author space parameter)
  (behavior:process (behavior-of self) msgtype author space parameter))

(defmethod respond ((self hexameter-context) &optional (tries *recv-tries*))
  (medium:respond (medium-of self) tries))


;;; Communication patterns
;;; ======================

(defun hex-item (&rest args)
  (apply 'make-item args))

(defmethod ask ((self hexameter-context) msgtype recipient space parameter)
  (let ((aphrodisiac (make-hash-table :test 'equalp))
        (response nil)
        (response-p nil))
    (setf (gethash "author" aphrodisiac) recipient)
    (setf (gethash "space" aphrodisiac) space)
    (process self "put" (me self) "net.lust" (list aphrodisiac))
    (tell self msgtype recipient space parameter)
    (while (not response-p)
      (respond self 0)
      (multiple-value-bind (resp resp-p) (process self "get" (me self) "net.lust" (list aphrodisiac))
        (setf response resp)
        (setf response-p resp-p)))
    response))
