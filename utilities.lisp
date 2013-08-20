(in-package #:hexameter-utilities)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (module-manager:load-module :gbbopen-tools))

(defmacro eval-always (&body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@body))

(defmacro dohash ((key value hash) &body body)
  (with-once-only-bindings (hash)
    `(maphash (lambda (,key ,value) ,@body) ,hash)))

(define-condition instantiating-abstract-class-error (error)
  ((class :initarg :class :initform (required-argument :class)))
  (:report (lambda (condition stream)
             (format stream "Cannot instantiate abstract class ~A."
                     (slot-value condition 'class)))))

(defmacro make-class-abstract (class)
  (with-once-only-bindings (class)
    `(defmethod make-instance :around
         ((self (eql (find-class ,class))) &key &allow-other-keys)
       (error 'instantiating-abstract-class-error :class ,class))))

(defmacro define-abstract-class (name supers slots &rest options)
  `(progn
     (define-class ,name ,supers ,slots ,@options)
     (make-class-abstract ',name)))

(defun normalize-to-keyword (string-designator)
  (cond ((keywordp string-designator)
         string-designator)
        ((symbolp string-designator)
         (make-keyword string-designator))
        ((stringp string-designator)
         (make-keyword (string-upcase string-designator)))
        (t
         (error "~A is not a string, symbol or keyword." 
                string-designator))))
