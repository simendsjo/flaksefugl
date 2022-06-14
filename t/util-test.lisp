(in-package :flaksefugl/test)

(def-suite util-tests
  :in all-tests)

(in-suite util-tests)

(test negate-test
  (is (uot::negate  0)  0)
  (is (uot::negate  1) -1)
  (is (uot::negate -1)  1))
