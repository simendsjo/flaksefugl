(cl:defpackage :flaksefugl
  (:use :cl)
  (:export #:flaksefugl
           #:start
           #:stop))

(cl:in-package :flaksefugl)

(defvar *gravity* (gamekit:vec2 0.0 -0.1))
(defvar *speed* (gamekit:vec2 0.0 0.0))
(defvar *pos* (gamekit:vec2 0.0 480.0))
(defvar *flaksespeed* (gamekit:vec2 0.0 5.0))

(gamekit:defgame flaksefugl ()
  ()
  (:viewport-width 800)
  (:viewport-height 600)
  (:viewport-title "Flaksefugl"))

(gamekit:register-resource-package :keyword
                                   (asdf:system-relative-pathname :flaksefugl "assets/megacrash.itch.io/source/"))

(gamekit:define-image :bird "Player/bird1.png")

(defmethod gamekit:post-initialize ((app flaksefugl))
  (gamekit:bind-button :up :pressed (lambda () (setf *speed* (gamekit:add *speed* *flaksespeed*)))))

(defmethod gamekit:draw ((app flaksefugl))
  (gamekit:draw-image *pos* :bird))

(defmethod gamekit:act ((app flaksefugl))
  (setf *speed* (gamekit:add *speed* *gravity*))
  (setf *pos* (gamekit:add *pos* *speed*)))

(defun start ()
  (gamekit:start 'flaksefugl))

(defun stop ()
  (gamekit:stop))
