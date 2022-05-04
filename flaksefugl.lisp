(cl:defpackage :flaksefugl
  (:use :cl :alexandria :serapeum)
  (:local-nicknames (#:gk #:trivial-gamekit)
                    (#:a #:alexandria)
                    (#:t #:trivia))
  (:export #:flaksefugl
           #:start
           #:stop))

(cl:in-package :flaksefugl)

(defvar *white* (gk:vec4 1 1 1 1))

(defvar *random* (make-random-state t)
  "Random state")
(defvar *paused* nil
  "If T, ACT will not progress the game state.")
(defvar *gameover* nil
  "If T, the game is finished.")

(defvar *size* (gk:vec2 256 256)
  "Size of the screen. Matches size of background image.")
(defvar *size/2* (gk:div *size* 2)
  "Midpoint of size.")

(defvar *birdsize* (gk:vec2 16 16)
  "Size of bird. Matches image.")
(defvar *birdsize/2* (gk:div *birdsize* 2)
  "Midpoint of bird.")

;; We set these in RESET to easily restart the game and support a more repl
;; driven development cycle
(defvar *gravity* nil
  "Speed added to *SPEED* each tick.")
(defvar *speed* nil
  "Current speed added to *POS* each tick.")
(defvar *min-speed* (gk:vec2 -10 -10)
  "Maximum negative *SPEED* values.")
(defvar *max-speed* (gk:vec2 10 4)
  "Maximum positive *SPEED* values.")
(defvar *pos* nil
  "Position of bird in world coordinates.")
(defvar *flaksespeed* nil
  "Speed added to *POS* when the bird flaps its wings.")
(defvar *camera* nil
  "Position of camera. Things in world coordinates is translated to screen using this value.")
(defvar *background-speed* nil
  "Speed of background image to give parallax effect.")
(defvar *pipes* nil
  "The pipes in the game.")
(defvar *level* nil
  "Current level. Used for difficulty and scoring.")
(defvar *level-complete* nil
  "T iff the level is currently completed.")
(defvar *score* nil
  "Current score.")

(defstruct level
  "Data for level"
  space-between
  opening)

(defstruct pipe
  "Complete vertical pipe, both TOP part and BOTTOM part. Both are SIZE height,
so BOTTOM is placed below the screen, and TOP will extend the screen to the top."
  bottom
  top)

(defvar *pipe-width* 32
  "Width of a pipe. Matches the pipe image width.")

(defun world->screen (world)
  "Translates a world coordinate to screen coordinate."
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
  "Clamps the VEC2, VEC, between the MIN VEC2 and MAX VEC2."
  (gk:vec2 (clamp (gk:x vec) (gk:x min) (gk:x max))
           (clamp (gk:y vec) (gk:y min) (gk:y max))))

(defun flakse ()
  "Flap bird wings. Adds *FLAKSESPEED* to *SPEED* while keeping it within
reasonable bounds."
  (setf *speed* (clamp-vec (gk:add *speed* *flaksespeed*)
                           *min-speed*
                           *max-speed*)))

(defun toggle-pause ()
  "Toggle *PAUSED*."
  (setf *paused* (not *paused*)))

(defmethod gk:post-initialize ((app flaksefugl))
  (reset)
  (gk:bind-button :enter :pressed (lambda () (reset)))
  (gk:bind-button :space :pressed (lambda ()
                                    (when *level-complete*
                                      (setf *speed* (gk:add *speed* (gk:vec2 0.25 0))
                                            *level-complete* nil)
                                      (make-pipes))
                                    (toggle-pause)))
  (gk:bind-button :up :pressed (lambda () (flakse))))

;; FIXME: We draw UP TO *size* height, but don't record it. For the bottom pipe,
;; it might mean we collide above the pipe!
(defun draw-pipe (pos)
  "Draws a pipe starting at POS up to *SIZE* height."
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
  (let* ((start (floor (* (gk:x *camera*) (gk:x *background-speed*))))
         (diff (- (gk:x *camera*) start))
         (pages (floor (/ diff (gk:x *size*))))
         (x (+ start (* pages (gk:x *size*)))))
    (gk:draw-image (world->screen (gk:vec2 x 0)) :background)
    (gk:draw-image (world->screen (gk:vec2 (+ x (gk:x *size*)) 0)) :background))
  ;; pipes
  (do-each (p *pipes*)
    (draw-pipe (pipe-bottom p))
    (draw-pipe (pipe-top p)))
  ;; bird
  (gk:draw-image (world->screen *pos*) :bird :width (gk:x *birdsize*) :height (gk:x *birdsize*))
  ;; score
  (gk:draw-text (format nil "Score: ~A" *score*) (gk:vec2 10 (- (gk:y *size*) 20)) :fill-color *white*)
  (cond
    (*gameover* (draw-centered-text "GAME OVER :( ENTER to restart."))
    (*level-complete* (draw-centered-text "Good job! Press SPACE to continue."))
    (*paused* (draw-centered-text "PAUSED. SPACE to unpause."))))

(defun draw-centered-text (txt)
  "Draw TXT at the center of the screen."
  (multiple-value-bind (origin width height advance) (gk:calc-text-bounds txt)
    (gk:draw-text (format nil txt) (gk:vec2 (/ (- (gk:x *size*) width) 2) (gk:y *size/2*)) :fill-color *white*)))

(defun overlapsp (a b)
  "T iff the VEC4 A overlaps with the VEC4 b."
  (let ((ax (gk:x a))
        (ay (gk:y a))
        (axe (gk:z a))
        (aye (gk:w a))
        (bx (gk:x b))
        (by (gk:y b))
        (bxe (gk:z b))
        (bye (gk:w b)))
    (and (>= axe bx)
         (>= aye by)
         (<= ax bxe)
         (<= ay bye))))

(defun pipebox (pipe)
  "Bounding box for pipe starting at position PIPE."
  (let ((x (gk:x pipe))
        (y (gk:y pipe)))
    (gk:vec4 x
             y
             (+ x *pipe-width*)
             (+ y (gk:y *size*)))))

(defun birdbox ()
  "Bounding box for bird."
  (let ((x (gk:x *pos*))
        (y (gk:y *pos*)))
    (gk:vec4 x
             y
             (+ x (gk:x *birdsize*))
             (+ y (gk:y *birdsize*)))))

(defun bird-collided-p ()
  "T iff the bird collides with any pipe or top/bottom of the screen."
  (let ((birdbox (birdbox)))
    ;; screen top
    (when (>= (gk:w birdbox) (gk:y *size*))
      (return-from bird-collided-p t))
    ;; screen bottom
    (when (<= (gk:y *pos*) 0)
      (return-from bird-collided-p t))
    ;; pipes
    (do-each (p *pipes*)
      (when (or (overlapsp birdbox (pipebox (pipe-bottom p)))
                (overlapsp birdbox (pipebox (pipe-top p))))
        (return-from bird-collided-p t)))))

(defmethod gk:act ((app flaksefugl))
  (unless (or *gameover* *paused*)
    (setf *score* (floor (/ (gk:x *pos*) 10))
          *speed* (gk:add *speed* *gravity*)
          *pos* (gk:add *pos* *speed*)
          (gk:x *camera*) (- (gk:x *pos*) (gk:x *size/2*))
          *gameover* (bird-collided-p))
    (let ((last-pipe (aref *pipes* (- (array-dimension *pipes* 0) 1))))
      (when (> (gk:x *pos*) (+ (gk:x (pipe-bottom last-pipe)) *pipe-width*))
        (toggle-pause)
        (setf *level-complete* t)))))

(defun make-pipes ()
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

(defun reset ()
  "Reset game."
  (setf *gravity* (gk:vec2 0.0 -0.1)
        *speed* (gk:vec2 1.0 0.0)
        *pos* *size/2*
        *flaksespeed* (gk:vec2 0.0 3.0)
        *camera* (gk:subt *pos* *size/2*)
        *background-speed* (gk:vec2 0.25 0.0)
        *level* (make-level :space-between (gk:vec2 (* *pipe-width* 3) (* *pipe-width* 7)) :opening (gk:vec2 (* (gk:y *birdsize*) 3) (* (gk:y *birdsize*) 7)))
        *level-complete* nil
        *score* 0
        *gameover* nil
        *pipes* (make-array 32 :element-type 'pipe))
  (make-pipes))

(defun start ()
  (gk:start 'flaksefugl))

(defun stop ()
  (gk:stop))
