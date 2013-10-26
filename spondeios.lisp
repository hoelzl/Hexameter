(in-package #:spondeios-impl)

(defvar *default-space* 'verbose-memory-space
  "The default space used to react to messages.")

(defvar *default-spheres* (list 'flagging-sphere 'networking-sphere 'verbose-sphere)
  "The default spheres used to process messsages before passing them to the space.")


;;; Helper functions
;;; ================

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

(defun printhash (hash)
  (format t "~&{ ")
  (maphash (lambda (key val) (format t "~A:~A [~A]" key val (type-of key))) hash)
  (format t "}"))

(defun normalize-param (param builder)
  (cond ((typep param 'symbol)
         (funcall builder param))
        ((or (typep param 'hexameter-space) (typep param 'hexameter-sphere)) ; NOTE: while semantically different, spheres and spaces are interchangeable technically
         param)
        (t
         (warn "Unsafe parameter to spondeios")
         param)))


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
   (processor :type context-dependent
              :documentation "Function for inward message handling.")
   (actor :type context-dependent
          :documentation "Function for outward message handling.")))

(defmethod initialize-instance
    :after ((self spondeios-context)
            &key (space *default-space*) (spheres *default-spheres*))
  (let ((processor (normalize-param space (lambda (space-class) (make-instance space-class :context self)))))
    (dolist (sphere spheres)
      (setf processor
            (normalize-param sphere
                             (lambda (sphere-class)
                               (make-instance sphere-class
                                 :context self
                                 :continuation processor
                                 :direction :in)))))
    (setf (processor-of self) processor))
  (let ((actor (make-instance 'message-space :context self)))
    (dolist (sphere (reverse spheres))
      (setf actor
            (normalize-param sphere
                             (lambda (sphere-class)
                               (make-instance sphere-class
                                 :context self
                                 :continuation actor
                                 :direction :out)))))
    (setf (actor-of self) actor)))

(defmethod couple ((self spondeios-context) message)
  (setf (message-of self) message))

(defmethod term ((self spondeios-context))
  (setf (message-of self) nil)
  (setf (processor-of self) nil)
  (setf (actor-of self) nil)
  t)

(defmethod process ((self spondeios-context) msgtype author space parameter
                    &optional recipient)
  (funcall #'handle (processor-of self) msgtype author space parameter
           recipient))

(defmethod act ((self spondeios-context) msgtype recipient space parameter
                &optional author)
  (cond ((equal recipient (me self))
         (process self msgtype recipient space parameter author)
         t)
        (t
         (handle (actor-of self) msgtype recipient space parameter
                  author))))


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

;; TODO: Implement comparison of tuple contents, making a query like {a=42} match a saved tuple like {a=42, b=43}
(define-class memory-space (hexameter-space)
  ((memory :initform (make-hash-table :test 'equalp))))

(defmethod handle ((self memory-space) msgtype author space parameter
                   &optional recipient)
  (declare (ignore author recipient))
  (setf msgtype (normalize-to-keyword msgtype))
  (ecase-using equal msgtype
    ((:get :qry)
     (let* ((memory (gethash space (memory-of self) (make-hash-table :test 'equalp)))
           (response (list)))
       (when memory
         (dolist (item parameter)
           (when (gethash item memory)
             (push item response)
             (when (equal msgtype :get)
               (remhash item memory)))))
       (values response t)))
    ((:put)
     (let ((memory (ensure-gethash space (memory-of self) (make-hash-table :test 'equalp))))
       (dolist (item parameter)
         (setf (gethash item memory) t)))
     (values parameter t))))

(define-class verbose-memory-space (memory-space)
  ())

(defmethod handle :before ((self verbose-memory-space) msgtype author space parameter
                           &optional recipient)
  (format t "~&**  Requested a ~A at ~A from ~A" msgtype space author))

;; NOTE: this exists for test purposes for now
(define-class constant-space (hexameter-space)
  ())

(defmethod handle ((self constant-space) msgtype author space parameter
                   &optional recipient)
  (values (list (make-item :answer 42)) t))

;;; TODO: Move the following definitions to a more sensible place.
(define-unit-class answer ()
  (space author value))

(define-unit-class wrapped-value ()
  (value))

#+ (or)
(define-unit-class formula ()
  (contents))

(defmethod print-instance-slots :after ((self wrapped-value) stream)
  (format stream " Value:")
  (print-instance-slot-value self 'value stream))

(defgeneric wrap (value)
  (:method ((self wrapped-value))
    (warn "Wrapping already wrapped value ~A." self)
    self)
  #+ (or)
  (:method ((self hash-table))
    (if (eql (gethash :type self nil) :formula)
        (make-instance 'formula :contents (gethash :contents self 'nothing))
        (make-instance 'wrapped-value :value self)))
  (:method (self)
    (make-instance 'wrapped-value :value self)))

(defgeneric unwrap (wrapped-value)
  (:method ((self wrapped-value))
    (value-of self))
  (:method (self)
    (warn "Unwrapping non-wrapped value ~A." self)
    self))

;;; NOTE: Do not use with flagging enabled (you want to be flag-transparent)
(define-class blackboard-space (hexameter-space)
  ((translate-tuple-to-unit :initform 'wrap) 
   (translate-unit-to-tuple :initform 'unwrap)))

(defmethod handle ((self blackboard-space) msgtype author space parameter &optional recipient)
  (declare (ignore recipient))
  (setf msgtype (normalize-to-keyword msgtype))
  (ecase-using equal msgtype
    ((:get :qry)
     ;; TODO: Use FIND-INSTANCES with an appropriate pattern
     (let ((answers (find-instances-of-class 'answer)))
       (when (eql msgtype :get)
         (mapc 'delete-instance answers))
       (values (mapcar (translate-unit-to-tuple-of self) answers) t)))
    ((:put)
     (let ((translator (translate-tuple-to-unit-of self)))
       (dolist (item parameter)
         (funcall translator item))
       (values parameter t)))))

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

(define-class flagging-sphere (hexameter-sphere)
  ())

;; NOTE: Currently only supports space flagging
;; TODO: Implement tuple-flagging
(defmethod handle ((self flagging-sphere) msgtype author/recipient space parameter
                   &optional recipient)
  (cond ((eql (direction-of self) :in)
         (handle (continuation-of self) msgtype author/recipient (first (cl-ppcre:split "#" space)) parameter))
        ((eql (direction-of self) :out)
         (handle (continuation-of self) msgtype author/recipient space parameter))))

;; TODO: Support net.friends
(define-class networking-sphere (hexameter-sphere)
  ((lust :initform (make-hash-table :test 'equalp))
   (friends :initform (make-hash-table :test 'equalp))))

(defmethod handle ((self networking-sphere) msgtype author/recipient space parameter
                   &optional recipient)
  (cond ((eql (direction-of self) :in)
         (let ((author author/recipient))
           (if (string= msgtype "ack")
               (multiple-value-bind (authorlust authorlust-p) (gethash author (lust-of self))
                 (if authorlust-p
                     (multiple-value-bind (spacelust spacelust-p) (gethash space authorlust)
                       (if spacelust-p
                           (dolist (item parameter)
                             (push item (gethash space authorlust)))
                           ()))
                     ())
                 (values nil nil))
               (cond ((string= space "net.lust")
                      (cond ((string= msgtype "get")
                             (let ((response nil)
                                   (response-p nil))
                               (dolist (item parameter)
                                 (multiple-value-bind (author author-p) (gethash "author" item)
                                   (multiple-value-bind (space space-p) (gethash "space" item)
                                     (if (and author-p space-p)
                                         (multiple-value-bind (authorlust authorlust-p) (gethash author (lust-of self))
                                           (if authorlust-p
                                               (multiple-value-bind (spacelust spacelust-p) (gethash space authorlust)
                                                 (if spacelust-p
                                                     (progn
                                                       (setf response-p t)
                                                       (dolist (answer spacelust)
                                                         (push answer response)))
                                                     ()))
                                               ()))
                                         ()))))
                               (values response response-p)))
                            ((string= msgtype "put")
                             (dolist (item parameter)
                               (multiple-value-bind (author author-p) (gethash "author" item)
                                 (multiple-value-bind (space space-p) (gethash "space" item)
                                   (if (and author-p space-p)
                                       (let ((authorlust (gethash author (lust-of self))))
                                         (setf (gethash author (lust-of self)) (if authorlust (gethash author (lust-of self)) (make-hash-table :test 'equalp)))
                                         (setf (gethash space authorlust) nil))
                                       ()))))
                             (values parameter t))
                            ((string= msgtype "qry")
                             (values nil t))))
                     (t
                      (handle (continuation-of self) msgtype author space parameter))))))
        ((eql (direction-of self) :out)
         (handle (continuation-of self) msgtype author/recipient space parameter))))
