(cl:defpackage :flaksefugl
  (:use :cl)
  (:local-nicknames (#:gk #:trivial-gamekit))
  (:export #:flaksefugl
           #:start
           #:stop))

(cl:in-package :flaksefugl)

(defvar *width* 256)
(defvar *height* 256)

;; We set these in RESET to easily restart the game and support a more repl
;; driven development cycle
(defvar *gravity* nil)
(defvar *speed* nil)
(defvar *pos* nil)
(defvar *flaksespeed* nil)

(gk:defgame flaksefugl ()
  ()
  (:viewport-width *width*)
  (:viewport-height *height*)
  (:viewport-title "Flaksefugl"))

(gk:register-resource-package :keyword
                                   (asdf:system-relative-pathname :flaksefugl "assets/megacrash.itch.io/source/"))

(gk:define-image :bird "Player/bird1.png")
(gk:define-image :background "Background/Background4.png")

(defmethod gk:post-initialize ((app flaksefugl))
  (reset)
  (gk:bind-button :f5 :pressed (lambda () (reset)))
  (gk:bind-button :up :pressed (lambda () (setf *speed* (gk:add *speed* *flaksespeed*)))))

(defmethod gk:draw ((app flaksefugl))
  (gk:draw-image (gk:vec2 0 0) :background)
  (gk:draw-image *pos* :bird))

(defmethod gk:act ((app flaksefugl))
  (setf *speed* (gk:add *speed* *gravity*))
  (setf *pos* (gk:add *pos* *speed*)))

(defun reset ()
  (setf *gravity* (gk:vec2 0.0 -0.1))
  (setf *speed* (gk:vec2 1.0 0.0))
  (setf *pos* (gk:vec2 (/ *width* 2) (/ *height* 2)))
  (setf *flaksespeed* (gk:vec2 0.0 5.0)))

(defun start ()
  (gk:start 'flaksefugl))

(defun stop ()
  (gk:stop))
