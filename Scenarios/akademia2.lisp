(require :hexameter)
(in-package :hexameter-impl)
(define-class logic-space (spondeios:hexameter-space)
  ())

;; (defmethod spondeios:handle ((self logic-space) msgtype author space parameter
;;                    &optional recipient)
;;   (values
;;    (list (hex:hex-item 
;;           :answer (list (hex-item 
;;                          :type "move"
;;                          :control (hex-item
;;                                    :up 1))
;;                         (hex-item
;;                          :type "shout"
;;                          :control (hex-item
;;                                    :content (hex-item
;;                                              :class "commit goal"
;;                                              :live (list (hex-item
;;                                                           :class "motor"
;;                                                           :type "move"
;;                                                           :control (hex-item
;;                                                                     :right 1)))))))))
;;    t))
(defmethod spondeios:handle ((self logic-space) msgtype author space parameter
                   &optional recipient)
  (values
   (list (hex:hex-item 
          :answer (list (hex-item 
                         :type "move"
                         :control (hex-item
                                   :up 1))
                        (hex-item
                         :type "shout"
                         :control (hex-item
                                   :content (hex-item
                                             :class "commit goal"
                                             :live (list (hex-item
                                                          :class "commit feature"
                                                          1 (hex-item :on (hex-item :x 7)
                                                                    :do (hex-item
                                                                         :class "motor"
                                                                         :type "move"
                                                                         :control (hex-item
                                                                                   :up 1)))
                                                          :otherwise (hex-item
                                                                      :class "motor"
                                                                      :type "move"
                                                                      :control (hex-item
                                                                                :right 1))))))))))
   t))
(let ((context (hex:init :me "localhost:55559" :space 'logic-space)))
  (format t "~&Entering infinite response loop, please abort manually.")
  (loop while t do (hex:respond context 0)))