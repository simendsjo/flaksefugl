(cl:defpackage :flaksefugl
  (:use :cl)
  (:local-nicknames (#:gk #:trivial-gamekit))
  (:export #:flaksefugl
           #:start
           #:stop))

(cl:in-package :flaksefugl)

(defvar *gravity* (gk:vec2 0.0 -0.1))
(defvar *speed* (gk:vec2 0.0 0.0))
(defvar *pos* (gk:vec2 0.0 480.0))
(defvar *flaksespeed* (gk:vec2 0.0 5.0))

(gk:defgame flaksefugl ()
  ()
  (:viewport-width 800)
  (:viewport-height 600)
  (:viewport-title "Flaksefugl"))

(gk:register-resource-package :keyword
                                   (asdf:system-relative-pathname :flaksefugl "assets/megacrash.itch.io/source/"))

(gk:define-image :bird "Player/bird1.png")

(defmethod gk:post-initialize ((app flaksefugl))
  (gk:bind-button :up :pressed (lambda () (setf *speed* (gk:add *speed* *flaksespeed*)))))

(defmethod gk:draw ((app flaksefugl))
  (gk:draw-image *pos* :bird))

(defmethod gk:act ((app flaksefugl))
  (setf *speed* (gk:add *speed* *gravity*))
  (setf *pos* (gk:add *pos* *speed*)))

(defun start ()
  (gk:start 'flaksefugl))

(defun stop ()
  (gk:stop))
