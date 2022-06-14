;; Ref https://gitlab.common-lisp.net/asdf/asdf/blob/master/doc/best_practices.md

(defsystem "flaksefugl"
  :description "Flaksefugl"
  :author "Simen Endsjø <simendsjo@gmail.com>"
  :version "0.1.0"
  :depends-on ("alexandria" "serapeum" "trivia" "trivial-gamekit")
  :pathname "src/"
  :serial t
  :components ((:file "packages")
               (:file "utils")
               (:file "flaksefugl"))
  :in-order-to ((test-op (test-op :flaksefugl/test))))

;; Ref http://turtleware.eu/posts/Tutorial-Working-with-FiveAM.html
;; https://github.com/uint/quasirpg/blob/test/quasirpg.asd
(defsystem "flaksefugl/test"
  :depends-on ("flaksefugl" "fiveam")
  :pathname "t/"
  :serial t
  :components ((:file "packages")
               (:file "setup")
               (:file "util-test"))
  :perform (test-op (o c) (symbol-call :flaksefugl/test :test-flaksefugl)))
