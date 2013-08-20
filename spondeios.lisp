(in-package #:spondeios-impl)

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


(define-abstract-class hexameter-space ()
  ())


(define-class trivial-space (hexameter-space funcallable-standard-object)
  ()
  (:metaclass funcallable-standard-class))

(defun trivial-space-fun (self)
  (declare (ignore self))
  (lambda (type &optional author space parameter)
    (format nil "Answer to ~W of type ~W from ~A@~A."
            parameter type author space)))

(defmethod initialize-instance :after ((self trivial-space) &key)
  (set-funcallable-instance-function self (trivial-space-fun self)))

(defun trivial-space ()
  (make-instance 'trivial-space))


(define-class memory-space (hexameter-space funcallable-standard-object)
  ((memory :initform (make-hash-table :test 'equalp)))
  (:metaclass funcallable-standard-class))

(defun memory-space-fun (self)
  (lambda (type &optional author space parameter)
    (declare (ignore author))
    (setf type (normalize-to-keyword type))
    (ecase-using equal type
      ((:get :qry)
       (let ((memory (gethash space (memory-of self) (make-hash-table :test 'equalp)))
             (response '()))
         (when memory
           (dolist (item parameter)
             (when (gethash item memory)
               (push item response)
               (when (equal type :get)
                 (remhash item memory)))))
         response))
      ((:put)
       (let ((memory (ensure-gethash space (memory-of self) (make-hash-table :test 'equalp))))
         (dolist (item parameter)
           (setf (gethash item memory) t)))
       parameter))))

(defmethod initialize-instance :after ((self memory-space) &key)
  (set-funcallable-instance-function self (memory-space-fun self)))

(defun memory-space ()
  (make-instance 'memory-space))
