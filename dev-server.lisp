;; I'm unable to get --script to work properly
;; Run the ./dev script to start
(asdf:load-system :slynk)
(slynk:create-server :port 4008)

(load "flaksefugl.asd")
(ql:quickload :flaksefugl)
(in-package :flaksefugl)
(start)
