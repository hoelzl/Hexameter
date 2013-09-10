(in-package #:spondeios-impl)

(defvar *default-space* 'memory-space)

(defvar *default-spheres* (list 'verbose-sphere))          ;spheres raise errors

(defun remove-matching-from-table (hash pattern)
  (check-type hash hash-table)
  (let* ((test (hash-table-test hash))
         (matching (make-hash-table :test test))
         (non-matching (make-hash-table :test test)))
    (dohash (key value hash)
      (if (and (stringp key)
               (ppcre:scan pattern key))
          (setf (gethash key matching) value)
          (setf (gethash key non-matching) value)))
    (values non-matching matching)))

(defgeneric friends-of (net))
;;; (defgeneric desires-of (net))
(defgeneric lust-of (net))

(define-class net ()
  ((friends :initform '())
   ;; (desires :initform '())
   (lust :initform '())))


;;; Context
;;; =======

(defun init (&rest args)
  (apply 'make-instance 'spondeios-context args))

(define-class spondeios-context ()
  ((me :accessor me :type string :initform "localhost"
       :documentation "The network name of the context.")
   (message :type message-space
            :initform (compose (constantly t) #'process)
            :documentation "The function to send messages.")
   (net :type net
        :documentation "Network state.")
   (processor :type context-dependent
              :documentation "Function for inward message handling.")
   (actor :type context-dependent
          :documentation "Function for outward message handling.")))

(defmethod initialize-instance
    :after ((self spondeios-context)
            &key (space *default-space*) (spheres *default-spheres*))
  (let ((processor (make-instance space :context self)))
    (dolist (sphere spheres)
      (setf processor (make-instance sphere
                        :context self
                        :continuation processor
                        :direction :in)))
    (setf (processor-of self) processor))
  (let ((actor (make-instance 'message-space :context self)))
    (dolist (sphere (reverse spheres))
      (setf actor (make-instance sphere
                    :context self
                    :continuation actor
                    :direction :out)))
    (setf (actor-of self) actor)))

(defmethod couple ((self spondeios-context) message)
  (setf (message-of self) message))

;;; Spaces
;;; ======


(define-abstract-class context-dependent ()
  ((context)))


(define-abstract-class hexameter-space (context-dependent)
  ())

(defgeneric handle (space-or-sphere msgtype author space parameter
                    &optional recipient)
  (:documentation "Handle GET, QRY and PUT requests for hexameter spaces and spheres."))

(define-class trivial-space (hexameter-space)
  ())

(defmethod handle ((self trivial-space) msgtype author space parameter
                   &optional recipient)
  (declare (ignore self recipient))
  (format nil "Answer to ~W of type ~W from ~A@~A."
          parameter msgtype author space))


(define-class message-space (hexameter-space)
  ())

(defmethod handle ((self message-space) msgtype recipient space parameter
                   &optional author)
  (funcall (message-of (context-of self)) msgtype recipient space parameter)
  t)

(define-class memory-space (hexameter-space)
  ((memory :initform (make-hash-table :test 'equalp))))

(defmethod handle ((self memory-space) msgtype author space parameter
                   &optional recipient)
  (declare (ignore author recipient))
  (setf msgtype (normalize-to-keyword msgtype))
  (ecase-using equal msgtype
    ((:get :qry)
     (let* ((memory (gethash space (memory-of space) (make-hash-table :test 'equalp)))
           (response '()))
       (when memory
         (dolist (item parameter)
           (when (gethash item memory)
             (push item response)
             (when (equal msgtype :get)
               (remhash item memory)))))
       response))
    ((:put)
     (let ((memory (ensure-gethash space (memory-of self) (make-hash-table :test 'equalp))))
       (dolist (item parameter)
         (setf (gethash item memory) t)))
     parameter)))


;;; Spheres
;;; =======

(define-abstract-class hexameter-sphere (context-dependent)
  ((continuation)
   (direction)))


(define-class id-sphere (hexameter-sphere)
  ())

(defmethod handle ((self id-sphere) msgtype author/recipient space parameter
                   &optional recipient)
  (handle (continuation-of self) msgtype author/recipient space parameter recipient))

(define-class verbose-sphere (hexameter-sphere)
  ())

(defmethod handle ((self verbose-sphere) msgtype author/recipient space parameter
                   &optional recipient)
  (cond ((eql (direction-of self) :in)
         (format t "~&--  [received ~A] ~A" msgtype parameter)
         (format t "~&--                 @~A from ~A" space author/recipient))
        ((eql (direction-of self) :out)
         (format t "~&++  [sent ~A] ~A" msgtype parameter)
         (format t "~&++                 @~A to ~A" space author/recipient)))
  (handle (continuation-of self) msgtype author/recipient space parameter recipient))

;;; Hexameter Interface
;;; ===================

(defmethod process ((self spondeios-context) msgtype author space parameter
                    &optional recipient)
  (funcall #'handle (processor-of self) msgtype author space parameter
           recipient))

(defmethod act ((self spondeios-context) msgtype recipient space parameter
                &optional author)
  (cond ((string= recipient (me self))
         (process self msgtype recipient space parameter author)
         t)
        (t
         (funcall #'handle (actor-of self) msgtype recipient space parameter
                  author))))
