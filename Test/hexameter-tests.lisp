(in-package #:hexameter-tests)
(in-suite :hexameter)

;; TODO: Use 5am fixtures here?
(defvar *ctx1* (progn (when (boundp '*ctx1*) (term *ctx1*)) (init :me "localhost:10001")))
(defvar *ctx2* (progn (when (boundp '*ctx2*) (term *ctx2*)) (init :me "localhost:10002")))

(defvar *msg1* (list (hex-item :a 42 :b 43)))
(defvar *msg2* (list (hex-item :a 42 :b 43 :x 1 :y 2)))

(test hexameter-message-reaches-target 
      (tell *ctx2* "put" "localhost:10001" "test" *msg1*)
      (respond *ctx1* 0))

(test hexameter-put-comes-through
      (multiple-value-bind (message success) (process *ctx1* "put" "localhost:10001" "test" *msg2*)
        (and (equal message *msg2*)) success))