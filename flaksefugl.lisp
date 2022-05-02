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
(defvar *level* nil)
(defvar *pipes* nil)
(defvar *score* 0)

(defstruct level
  space-between
  opening)

(defstruct pipe
  bottom
  top)

(defvar *pipe-width* 32)

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

(defun clamp-vec (vec min max)
  (gk:vec2 (clamp (gk:x vec) (gk:x min) (gk:x max))
           (clamp (gk:y vec) (gk:y min) (gk:y max))))

(defun flakse ()
  (setf *speed* (clamp-vec (gk:add *speed* *flaksespeed*)
                           (gk:vec2 -10 -10)
                           (gk:vec2 10 4))))

(defun toggle-pause ()
  (setf *paused* (not *paused*)))

(defmethod gk:post-initialize ((app flaksefugl))
  (reset)
  (gk:bind-button :enter :pressed (lambda () (reset)))
  (gk:bind-button :space :pressed (lambda () (toggle-pause)))
  (gk:bind-button :up :pressed (lambda () (flakse))))

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
  ;; background
  (let* ((page (floor (/ (gk:x *camera*) (gk:x *size*))))
         (x (* page (gk:x *size*))))
    (gk:draw-image (world->screen (gk:vec2 x 0)) :background)
    (gk:draw-image (world->screen (gk:vec2 (+ x (gk:x *size*)) 0)) :background))
  ;; pipes
  (do-each (p *pipes*)
    (draw-pipe (pipe-bottom p))
    (draw-pipe (pipe-top p)))
  ;; bird
  (gk:draw-image (world->screen *pos*) :bird :width (gk:x *birdsize*) :height (gk:x *birdsize*))
  ;; score
  (gk:draw-text (format nil "Score: ~A" *score*) (gk:vec2 10 (- (gk:y *size*) 20)) :fill-color (gk:vec4 1 1 1 1)))

(defmethod gk:act ((app flaksefugl))
  (unless *paused*
    (setf *score* (floor (/ (gk:x *pos*) 10)))
    (setf *speed* (gk:add *speed* *gravity*))
    (setf *pos* (gk:add *pos* *speed*))
    (setf (gk:x *camera*) (- (gk:x *pos*) (gk:x *size/2*)))))

(defun reset ()
  (setf *gravity* (gk:vec2 0.0 -0.1))
  (setf *speed* (gk:vec2 1.0 0.0))
  (setf *pos* *size/2*)
  (setf *flaksespeed* (gk:vec2 0.0 5.0))
  (setf *camera* (gk:subt *pos* *size/2*))
  (setf *level* (make-level :space-between (gk:vec2 (* *pipe-width* 3) (* *pipe-width* 7)) :opening (gk:vec2 (* (gk:y *birdsize*) 3) (* (gk:y *birdsize*) 7))))
  (setf *score* 0)
  ;; Spawn pipes
  (setf *pipes* (make-array 32 :element-type 'pipe))
  (let ((x (gk:x *size/2*)))
    (dotimes (i (array-dimension *pipes* 0))
      (let* ((opening (level-opening *level*))
             (min-opening (gk:x opening))
             (max-opening (gk:y opening))
             (rndopening (+ min-opening (random (- max-opening min-opening) *random-state*)))
             (bottom (- (random (- (gk:y *size*) rndopening) *random-state*) (gk:y *size*)))
             (top (+ bottom rndopening (gk:y *size*)))
             (between (level-space-between *level*))
             (min-between (gk:x between))
             (max-between (gk:y between))
             (rndbetween (+ min-between (random (- max-between min-between) *random-state*)))
             (newx (+ x rndbetween)))
        (setf x newx)
        (setf (aref *pipes* i) (make-pipe :bottom (gk:vec2 x bottom) :top (gk:vec2 x top)))))))

(defun start ()
  (gk:start 'flaksefugl))

(defun stop ()
  (gk:stop))
