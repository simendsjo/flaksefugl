(cl:defpackage :flaksefugl/test
  (:use :cl :5am)
  (:local-nicknames (#:uot #:flaksefugl))
  (:export :all-tests))

(in-package :flaksefugl/test)

(def-suite all-tests
  :description "All flaksefugl tests")

(in-suite all-tests)
