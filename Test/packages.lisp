(in-package :common-lisp-user)

(defpackage #:hexameter-test-suites
  (:use #:common-lisp
        #:5am))

(defpackage #:hexameter-utilities-tests
  (:use #:common-lisp
        #:5am
        #:gbbopen-tools
        #:hexameter-utilities))

(defpackage #:daktylos-tests
  (:use #:common-lisp
        #:5am
        #:gbbopen-tools
        #:hexameter-utilities
        #:daktylos-impl))

(defpackage #:spondeios-tests
  (:use #:common-lisp
        #:5am
        #:gbbopen-tools
        #:hexameter-utilities
        #:spondeios-impl))

(defpackage #:hexameter-tests
  (:use #:common-lisp
        #:5am
        #:gbbopen-tools
        #:hexameter-utilities
        #:hexameter-impl))
