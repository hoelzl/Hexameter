;;; packages.lisp

(in-package #:common-lisp-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *alexandria-imports*
    '(#:alist-hash-table
      #:alist-plist
      #:appendf
      #:array-index
      #:array-length
      #:binomial-coefficient
      #:circular-list
      #:circular-list-p
      #:circular-tree-p
      #:clamp
      #:coercef
      #:compose
      #:conjoin
      #:copy-array
      #:copy-hash-table
      #:copy-sequence
      #:count-permutations
      #:curry
      #:define-constant
      #:delete-from-plist
      #:delete-from-plistf
      #:deletef
      #:destructuring-case 
      #:destructuring-ccase
      #:destructuring-ecase
      #:disjoin
      #:emptyp
      #:ends-with
      #:ends-with-subseq
      #:ensure-car
      #:ensure-cons
      #:ensure-function
      #:ensure-functionf
      #:ensure-gethash
      #:ensure-symbol
      #:factorial
      #:first-elt
      #:flatten
      #:format-symbol
      #:gaussian-random
      #:hash-table-alist
      #:hash-table-keys 
      #:hash-table-values
      #:hash-table-plist
      #:if-let
      #:iota
      #:last-elt
      #:lastcar
      #:length=
      #:lerp
      #:map-combinations
      #:map-derangements
      #:map-iota
      #:map-permutations
      #:maphash-keys
      #:maphash-values
      #:make-circular-list
      #:make-gensym
      #:make-gensym-list
      #:mappend
      #:map-product
      #:maxf
      #:mean
      #:median
      #:minf
      #:multiple-value-prog2
      #:multiple-value-compose
      #:named-lambda
      #:nconcf
      #:nreversef
      #:nth-value-or
      #:nunionf
      #:of-type
      #:parse-body
      #:parse-ordinary-lambda-list
      #:plist-alist
      #:plist-hash-table
      #:proper-list
      #:proper-list-p
      #:proper-list-length
      #:proper-sequence
      #:random-elt
      #:rcurry
      #:read-file-into-string
      #:read-file-into-byte-vector
      #:remove-from-plist
      #:remove-from-plistf
      #:removef
      #:required-argument
      #:reversef
      #:rotate
      #:sequence-of-length-p
      #:shuffle
      #:standard-deviation
      #:starts-with
      #:starts-with-subseq
      #:string-designator
      #:subfactorial
      #:symbolicate
      #:type=
      #:unionf
      #:variance
      #:when-let
      #:when-let*
      #:whichever)))

(defpackage #:hexameter-utilities
  (:use #:common-lisp
        #:gbbopen-tools)
  (:nicknames #:hex-utils)
  (:import-from #:alexandria . #.*alexandria-imports*)
  (:export . #.*alexandria-imports*)
  (:export #:decode-json
           #:define-abstract-class
           #:dohash
           #:encode-alist-to-json
           #:encode-data-to-json
           #:encode-plist-to-json
           #:eval-always
           #:instantiating-abstract-class-error 
           #:make-class-abstract
           #:method-not-implemented-error
           #:normalize-to-keyword
           #:normalize-to-string
           #:make-item))

(defpackage #:daktylos-impl
  (:use #:common-lisp
        #:gbbopen-tools
        #:gbbopen
        #:hexameter-utilities)
  ;; Public exports
  (:export #:init #:term #:me #:couple
           #:message #:respond)
  ;; Private exports
  (:export #:hexameter-coder #:encode #:decode
           #:json-coder))

(defpackage #:daktylos
  (:nicknames #:medium)
  (:use #:daktylos-impl)
  (:export #:init #:term #:me #:couple
           #:message #:respond
           #:hexameter-coder
           #:json-coder))

(defpackage #:spondeios-impl
  (:use #:common-lisp
        #:gbbopen-tools
        #:gbbopen
        #:hexameter-utilities)
  ;; Public exports
  (:export #:init #:term #:me #:couple
           #:process #:act
           #:hexameter-space
           #:trivial-space #:memory-space #:verbose-memory-space
           #:handle
           #:hexameter-sphere
           #:id-sphere #:verbose-sphere
           #:flagging-sphere #:networking-sphere
           #:forwarding-sphere
           #:blackboard-space
           #:constant-space)
  ;; Private exports
  (:export))

(defpackage #:spondeios
  (:nicknames #:behavior)
  (:use #:spondeios-impl)
  (:export #:init #:term #:me #:couple
           #:process #:act
           #:hexameter-space
           #:handle
           #:trivial-space #:memory-space #:verbose-memory-space #:blackboard-space #:constant-space
           #:hexameter-sphere
           #:id-sphere #:verbose-sphere #:flagging-sphere #:networking-sphere #:forwarding-sphere))

(defpackage #:hexameter-impl
  (:use #:common-lisp
        #:gbbopen-tools
        #:gbbopen
        #:hexameter-utilities)
  ;; Public exports
  (:export #:init #:term #:me
           #:tell #:process #:respond
           #:ask #:meet #:friends
           #:hex-item)
  ;; Private exports
  (:export))

(defpackage #:hexameter
  (:use #:hexameter-impl)
  (:nicknames #:hex)
  (:export #:init #:term #:me
           #:tell #:process #:respond
           #:ask #:meet #:friends
           #:hex-item))

(defpackage #:hexameter-user
  (:use #:common-lisp
        #:gbbopen-tools
        #:hexameter
        #:hexameter-utilities)
  (:nicknames #:hex-user))
