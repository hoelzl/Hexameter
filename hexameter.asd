;;;; hexameter.asd

(in-package :asdf)

;;; We need the module manager to satisfy the gbbopen-tools dependency, but asdf complains about
;;; a circular dependency when trying to compile this file if gbbopen is already loaded.
;;;
(unless (member :module-manager *features*)
  (require '#:gbbopen))

(defsystem #:hexameter
  :serial t
  :description "The Implementation of the hexameter communication protocol"
  :version "0.0.1"
  :author '("Matthias Hoelzl <tc@xantira.com>"
            "Thomas Gabor <thomas@denkfrei.de>")
  :maintainer '("Matthias Hoelzl <tc@xantira.com>"
                "Thomas Gabor <thomas@denkfrei.de>")
  :license "MIT, see file LICENSE"
  :depends-on (;; The following three probably have to appear in this order.  Need to
               ;; investigate further.
               #:gbbopen
               #:module-manager
               #:gbbopen-tools
               #:gbbopen-core

               #:alexandria
               #:yason
               #:cl-ppcre 
               #:pzmq)
  :components ((:file "packages")
               (:file "utilities")
               (:file "spondeios")
               (:file "daktylos")
               (:file "hexameter")))
