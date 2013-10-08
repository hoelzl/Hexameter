(in-package #:hexameter-utilities)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (module-manager:load-module :gbbopen-tools))

(defmacro eval-always (&body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@body))

(defmacro dohash ((key value hash) &body body)
  (with-once-only-bindings (hash)
    `(maphash (lambda (,key ,value) ,@body) ,hash)))

(define-condition method-not-implemented-error (error)
  ((method :initarg :method :initform "<unknown method>")
   (arguments :initarg :arguments :initform "<unknown arguments>"))
  (:report (lambda (condition stream)
             (format stream "No method ~A implemented for arguments ~A."
                     (slot-value condition 'method)
                     (slot-value condition 'arguments)))))

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

(defun normalize-to-string (keyword &key (negligent nil))
  (cond ((stringp keyword)
         keyword)
        ((or (symbolp keyword) (keywordp keyword))
         (symbol-name keyword))
        (t
         (if negligent
             keyword
             (error "~A is not a a string, symbol, or keyword." keyword)))))

(defun normalize-to-keyword (string-designator &key (negligent nil))
  (cond ((keywordp string-designator)
         string-designator)
        ((symbolp string-designator)
         (make-keyword string-designator))
        ((stringp string-designator)
         (make-keyword (string-upcase string-designator)))
        (t
         (if negligent
             string-designator
             (error "~A is not a string, symbol or keyword." 
                    string-designator)))))

(defun make-item (&rest slots)
  (plist-hash-table (map 'list (lambda (slot) (normalize-to-string slot :negligent t)) slots) :test 'equalp))

(defmethod yason:encode ((data symbol) &optional stream)
  (yason:encode (string-downcase (symbol-name data)) stream))

(defun encode-data-to-json (data &optional stream)
  (if stream
      (yason:encode data stream)
      (with-output-to-string (stream)
        (yason:encode data stream))))

(defun encode-alist-to-json (data &optional stream)
  (if stream
      (yason:encode-alist data stream)
      (with-output-to-string (stream)
        (yason:encode-alist data stream))))

(defun encode-plist-to-json (data &optional stream)
  (if stream
      (yason:encode-plist data stream)
      (with-output-to-string (stream)
        (yason:encode-plist data stream))))

(defun decode-json (data)
  (yason:parse data))
