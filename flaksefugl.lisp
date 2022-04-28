(cl:defpackage :flaksefugl
  (:use :cl)
  (:export #:flaksefugl
           #:start
           #:stop))

(cl:in-package :flaksefugl)

(gamekit:defgame flaksefugl ()
  ()
  (:viewport-width 800)
  (:viewport-height 600)
  (:viewport-title "Flaksefugl"))

(defun start ()
  (gamekit:start 'flaksefugl))

(defun stop ()
  (gamekit:stop))
