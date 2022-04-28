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

(gamekit:register-resource-package :keyword
                                   (asdf:system-relative-pathname :flaksefugl "assets/megacrash.itch.io/source/"))

(gamekit:define-image :bird "Player/bird1.png")

(defmethod gamekit:draw ((app flaksefugl))
  (gamekit:draw-image (gamekit:vec2 0 0) :bird))

(defun start ()
  (gamekit:start 'flaksefugl))

(defun stop ()
  (gamekit:stop))
