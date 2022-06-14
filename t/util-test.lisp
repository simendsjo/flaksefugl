(in-package :flaksefugl/test)

(def-suite util-tests
  :in all-tests)

(in-suite util-tests)

(test negate-test
  (is (=  0 (uot::negate  0)))
  (is (= -1 (uot::negate  1)))
  (is (=  1 (uot::negate -1))))
