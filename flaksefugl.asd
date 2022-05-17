(defsystem "flaksefugl"
  :description "Flaksefugl"
  :author "Simen Endsjø <simendsjo@gmail.com>"
  :version "0.1.0"
  :depends-on ("alexandria" "serapeum" "trivia" "trivial-gamekit")
  :serial t
  :components ((:file "packages")
               (:file "utils")
               (:file "flaksefugl")))
