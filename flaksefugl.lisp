(cl:defpackage :flaksefugl
  (:use :cl :alexandria :serapeum)
  (:local-nicknames (#:gk #:trivial-gamekit))
  (:export #:flaksefugl
           #:start
           #:stop))

(cl:in-package :flaksefugl)


(defvar *random* (make-random-state t))
(defvar *paused* nil)

(defvar *size* (gk:vec2 256 256))
(defvar *size/2* (gk:div *size* 2))

(defvar *birdsize* (gk:vec2 16 16))
(defvar *birdsize/2* (gk:div *birdsize* 2))

;; We set these in RESET to easily restart the game and support a more repl
;; driven development cycle
(defvar *gravity* nil)
(defvar *speed* nil)
(defvar *pos* nil)
(defvar *flaksespeed* nil)
(defvar *camera* nil)

(defstruct pipe
  bottom
  top)

(defvar *pipe-width* 32)
(defvar *pipes* (make-array (+ 1 (ceiling (/ (gk:x *size*) *pipe-width*))) :element-type 'pipe))

(defun world->screen (world)
  (gk:subt world *camera*))

(gk:defgame flaksefugl ()
  ()
  (:viewport-width (gk:x *size*))
  (:viewport-height (gk:y *size*))
  (:viewport-title "Flaksefugl"))

(gk:register-resource-package :keyword
                                   (asdf:system-relative-pathname :flaksefugl "assets/megacrash.itch.io/source/"))

(gk:define-image :bird "Player/bird1.png")
(gk:define-image :background "Background/Background4.png")
(gk:define-image :pipe "Tileset/Style 1/PipeStyle1.png")

(defmethod gk:post-initialize ((app flaksefugl))
  (reset)
  (gk:bind-button :f5 :pressed (lambda () (reset)))
  (gk:bind-button :f12 :pressed (lambda () (setf *paused* (not *paused*))))
  (gk:bind-button :up :pressed (lambda () (setf *speed* (gk:add *speed* *flaksespeed*)))))

(defun draw-pipe (pos)
  (let ((pos (gk:vec2 (gk:x pos) (gk:y pos)))
        (edge-height 20)
        (mid-height 36))
    (flet ((incy (y) (setf (gk:y pos) (+ (gk:y pos) y))))
      ;; bottom
      (gk:draw-image (world->screen pos) :pipe :origin (gk:vec2 0 80) :width *pipe-width* :height edge-height)
      (incy edge-height)
      ;; middle
      (dotimes (i (floor (/ (- (gk:y *size*) (* 2 edge-height)) mid-height)))
        (gk:draw-image (world->screen pos) :pipe :origin (gk:vec2 0 100) :width *pipe-width* :height mid-height)
        (incy mid-height))
      ;; top
      (gk:draw-image (world->screen pos) :pipe :origin (gk:vec2 0 140) :width *pipe-width* :height edge-height)
      (incy edge-height))))

(defmethod gk:draw ((app flaksefugl))
  (let* ((page (floor (/ (gk:x *camera*) (gk:x *size*))))
         (x (* page (gk:x *size*))))
    (gk:draw-image (world->screen (gk:vec2 x 0)) :background)
    (gk:draw-image (world->screen (gk:vec2 (+ x (gk:x *size*)) 0)) :background))
  (do-each (p *pipes*)
    (draw-pipe (pipe-bottom p))
    (draw-pipe (pipe-top p)))
  (gk:draw-image (world->screen *pos*) :bird :width (gk:x *birdsize*) :height (gk:x *birdsize*)))

(defmethod gk:act ((app flaksefugl))
  (unless *paused*
    (setf *speed* (gk:add *speed* *gravity*))
    (setf *pos* (gk:add *pos* *speed*))
    (setf (gk:x *camera*) (- (gk:x *pos*) (gk:x *size/2*)))))

(defun reset ()
  (setf *gravity* (gk:vec2 0.0 -0.1))
  (setf *speed* (gk:vec2 1.0 0.0))
  (setf *pos* *size/2*)
  (setf *flaksespeed* (gk:vec2 0.0 5.0))
  (setf *camera* (gk:subt *pos* *size/2*))
  (dotimes (i (array-dimension *pipes* 0))
    (let* ((x (* i *pipe-width*))
           (bottom (- (random (gk:y *size/2*) *random-state*) (gk:y *size*)))
           (top (+ bottom (gk:y *size*) (random 60 *random-state*) *pipe-width*)))
      (setf (aref *pipes* i) (make-pipe :bottom (gk:vec2 x bottom) :top (gk:vec2 x top))))))

(defun start ()
  (gk:start 'flaksefugl))

(defun stop ()
  (gk:stop))
