(in-package #:hexameter-impl)

(defvar *default-host* "localhost")

(defvar *default-port* 55555
  "The default port for listening to requests.")

(defvar *default-space* 'behavior:memory-space)

(defvar *default-spheres* '('behavior:verbose-sphere)
  "The default spheres used to process messsages.")

(defvar *default-coder* :json)

(defvar *recv-tries* 10000
  "The number of times recv is called before aborting, if no data is received.")

(defun init (&rest args)
  (apply 'make-instance 'hexameter-context args))

(define-class hexameter-context ()
  ((me :accessor me :type string :initform (format nil "~A:~A" *default-host* *default-port*)
       :documentation "The network name of the context.")
   (medium :type daktylos-context
           :documentation "The context of the message transmission module.")
   (behavior :type spondeios-context
             :documentation "The context of the message processing module.")))

;; TODO: different types of me parameter need to be caught here
(defmethod initialize-instance
    :after ((self hexameter-context)
            &key (space *default-space*) (spheres *default-spheres*) (coder *default-coder*))
    (setf (behavior-of self) (behavior:init #|:space space :spheres spheres|# :me (me self)))
    (setf (medium-of self) (medium:init #|:coder coder|# :me (me self)))
    (let ((behavior-success
            (behavior:couple (behavior-of self)
                    (lambda (msgtype recipient space parameter)
                      (medium:message (medium-of self) msgtype recipient space parameter))))
          (medium-success
            (medium:couple (medium-of self)
                    (lambda (msgtype author space parameter)
                      (behavior:process (behavior-of self) msgtype author space parameter)))))
      (values medium-success behavior-success)))

(defmethod tell ((self hexameter-context) msgtype recipient space parameter)
  (behavior:act (behavior-of self) msgtype recipient space parameter))

(defmethod process ((self hexameter-context) msgtype author space parameter)
  (behavior:process (behavior-of self) msgtype author space parameter))

(defmethod respond ((self hexameter-context) &optional (tries *recv-tries*))
  (medium:respond (medium-of self) tries))
