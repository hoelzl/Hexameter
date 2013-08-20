(in-package #:daktylos-tests)
(in-suite :daktylos)

(test coder-is-abstract
  (signals instantiating-abstract-class-error
    (make-instance 'coder)))

(defvar *json-coder* (make-instance 'json-coder))

(test json-encode-numbers
  (is (string= (encode *json-coder* 123) "123"))
  (is (string= (encode *json-coder* -1) "-1"))
  (is (string= (encode *json-coder* 0) "0")))

(test json-encode-strings
  (is (string= (encode *json-coder* "") "\"\""))
  (is (string= (encode *json-coder* "abc") "\"abc\"")))

(test json-encode-lists
  (is (string= (encode *json-coder* '()) "null"))
  (is (string= (encode *json-coder* '(1 2 3)) "[1,2,3]"))
  (is (string= (encode *json-coder* '("foo" :bar baz))
               "[\"foo\",\"bar\",\"baz\"]")))

(test json-encode-keywords
  (is (string= (encode *json-coder* :foo) "\"foo\""))
  (is (string= (encode *json-coder* :true) "\"true\""))
  (is (string= (encode *json-coder* :false) "\"false\"")))

(test json-encode-symbols
  (is (string= (encode *json-coder* 'foo) "\"foo\""))
  (is (string= (encode *json-coder* 'true) "\"true\""))
  (is (string= (encode *json-coder* 'false) "\"false\"")))

(test json-decode-numbers
  (is (= (decode *json-coder* "123") 123))
  (is (= (decode *json-coder* "-1") -1))
  (is (= (decode *json-coder* "0") 0)))

(test json-decode-strings
  (is (string= (decode *json-coder* "\"\"") ""))  
  (is (string= (decode *json-coder* "\"abc\"") "abc"))  
  (is (string= (decode *json-coder* "\"false\"") "false")))
