(defsystem "flaksefugl"
  :description "Flaksefugl"
  :author "Simen Endsjø <simendsjo@gmail.com>"
  :version "0.1.0"
  :depends-on ("alexandria" "serapeum" "trivia" "trivial-gamekit")
  :serial t
  :components ((:file "packages")
               (:file "utils")
               (:file "flaksefugl"))
  :in-order-to ((test-op (test-op :flaksefugl/test))))

(defsystem "flaksefugl/test"
  :depends-on ("flaksefugl" "fiveam")
  :pathname "t/"
  :serial t
  :components ((:file "packages")
               (:file "setup")
               (:file "util-test"))
  :perform (test-op (o c) (symbol-call :5am :run!)))
