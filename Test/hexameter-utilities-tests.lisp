(in-package #:hexameter-utilities-tests)
(in-suite :hexameter-utilities)

(eval-always
  (defmacro dohash-test (name &rest key-value-pairs)
    `(test ,name
       (let ((hash (make-hash-table))
             (keys '())
             (values '()))
         ,@(mapcar (lambda (kv)
                     `(setf (gethash ',(first kv) hash) ',(second kv)))
                   key-value-pairs)
         (dohash (k v hash)
           (push k keys)
           (push v values))
         (is (= (length keys) (length values) ,(length key-value-pairs)))
         ,@(mapcar (lambda (kv)
                     `(is (=  (position ',(first kv) keys)
                              (position ',(second kv) values))))
                   key-value-pairs)))))

(dohash-test dohash-empty)
(dohash-test dohash-1 (:foo :bar))
(dohash-test dohash-2 (:foo :bar) (:baz :quux))
(dohash-test dohash-5 (one 1) (two 2) (three 3) (four 4) (five 5))

(define-abstract-class my-abstract-class ()
  ((foo :initform :foo)
   (bar)))

(define-class my-concrete-class (my-abstract-class)
  (baz))

(test abstract-class-1
  (signals instantiating-abstract-class-error
    (make-instance 'my-abstract-class)))

(test abstract-class-2
  (signals instantiating-abstract-class-error
    (make-instance 'my-abstract-class :foo 1 :bar 2)))

(test concrete-class-1
  (let ((obj (make-instance 'my-concrete-class)))
    (is (eql (foo-of obj) :foo))
    (is (not (slot-boundp obj 'bar)))
    (is (not (slot-boundp obj 'baz)))))

(test concrete-class-2
  (let ((obj (make-instance 'my-concrete-class
               :foo 1 :bar 2 :baz 3)))
    (is (eql (foo-of obj) 1))
    (is (eql (bar-of obj) 2))
    (is (eql (baz-of obj) 3))))

(test normalize-to-keyword-1
  (is (eq (normalize-to-keyword :foo) :foo))
  (is (eq (normalize-to-keyword 'foo) :foo))
  (is (eq (normalize-to-keyword "foo") :foo)))
