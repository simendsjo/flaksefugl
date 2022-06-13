;; I'm unable to get --script to work properly
;; Run `make dev-server` to start
(asdf:make "slynk")
(slynk:create-server :port 4008)

(ql:quickload "flaksefugl")
(in-package :flaksefugl)
(start)
