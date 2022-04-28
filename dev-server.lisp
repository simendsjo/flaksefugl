;; I'm unable to get --script to work properly
;; Run with
;;  LD_LIBRARY_PATH=$LIBRARY_PATH sbcl --load dev-server.lisp
(asdf:load-system :slynk)
(slynk:create-server :port 4008)

(load "flaksefugl.asd")
(ql:quickload :flaksefugl)
(in-package :flaksefugl)
(start)
