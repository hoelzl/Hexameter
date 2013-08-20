(in-package :common-lisp-user)

(defpackage #:hexameter-test-suites
  (:use #:common-lisp
        #:5am))

(defpackage #:hexameter-utilities-tests
  (:use #:common-lisp
        #:5am
        #:gbbopen-tools
        #:hexameter-utilities))

(defpackage #:spondeios-tests
  (:use #:common-lisp
        #:5am
        #:gbbopen-tools
        #:hexameter-utilities
        #:spondeios))

(defpackage #:daktylos-tests
  (:use #:common-lisp
        #:5am
        #:gbbopen-tools
        #:hexameter-utilities
        #:daktylos))

(defpackage #:hexameter-tests
  (:use #:common-lisp
        #:5am
        #:gbbopen-tools
        #:hexameter-utilities
        #:hexameter))
