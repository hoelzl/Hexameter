(in-package #:hexameter-impl)

(defvar *default-host* "localhost"
  "The default hostname to reach the component.")

(defvar *default-port* 55555
  "The default port for listening to requests.")

(defvar *default-space* 'behavior:verbose-memory-space
  "The default space used to react to messages.")

(defvar *default-spheres* (list 'behavior:flagging-sphere 'behavior:networking-sphere 'behavior:verbose-sphere)
  "The default spheres used to process messsages before passing them to the space.")

(defvar *default-coder* :json
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

;; TODO: different types of me parameter need to be caught here, currently only the default name is supported (i.e. don't use the me parameter yet!)
(defmethod initialize-instance
    :after ((self hexameter-context)
            &key (space *default-space*) (spheres *default-spheres*) (coder *default-coder*))
    (setf (behavior-of self) (behavior:init :space space :spheres spheres :me (me self)))
    (setf (medium-of self) (medium:init :coder coder :me (me self)))
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

;; NOTE: Due to a bug apparently located in the pzmq binding, this method MUST be called with optional parameter equalling 0
(defmethod respond ((self hexameter-context) &optional (tries *recv-tries*))
  (medium:respond (medium-of self) tries))


;;; Communication patterns
;;; ======================

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
