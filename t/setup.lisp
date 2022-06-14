(in-package :flaksefugl/test)

(def-suite all-tests
  :description "All flaksefugl tests")

(in-suite all-tests)

(defun test-flaksefugl ()
  (run! 'all-tests))
