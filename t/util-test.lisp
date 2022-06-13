(in-package :flaksefugl/test)

(test negate-test
  (is (uot::negate  0)  0)
  (is (uot::negate  1) -1)
  (is (uot::negate -1)  1))
