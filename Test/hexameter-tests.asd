;;;; hexameter.asd

(in-package :asdf)

;;; We need the module manager to satisfy the gbbopen-tools dependency, but asdf complains about
;;; a circular dependency when trying to compile this file if gbbopen is already loaded.
;;;
(unless (member :module-manager *features*)
  (require '#:gbbopen))

(defsystem #:hexameter-tests
  :serial t
  :description "Tests for hexameter"
  :version "0.0.1"
  :author '("Matthias Hoelzl <tc@xantira.com>"
            "Thomas Gabor <thomas@denkfrei.de>")
  :maintainer '("Matthias Hoelzl <tc@xantira.com>"
                "Thomas Gabor <thomas@denkfrei.de>")
  :license "MIT, see file LICENSE in the root directory"
  :depends-on (#:hexameter
               #:fiveam
               #:alexandria
               #:gbbopen
               #:module-manager
               #:gbbopen-tools
               #:zeromq)
  :components ((:file "packages")
               (:file "hexameter-test-suites")
               (:file "hexameter-utilities-tests")
	       (:file "spondeios-tests")
               (:file "daktylos-tests")
               (:file "hexameter-tests")))
