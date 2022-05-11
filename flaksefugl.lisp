(declaim (optimize (speed 0) (space 0) (debug 3)))
(cl:defpackage :flaksefugl
  (:use :cl :alexandria :serapeum)
  (:local-nicknames (#:gk #:trivial-gamekit)
                    (#:a #:alexandria)
                    (#:t #:trivia))
  (:export #:flaksefugl
           #:start
           #:stop))

(cl:in-package :flaksefugl)

(defun negate (x)
  "Negate X."
  (* x -1))

;; vectors are represented as arrays, so copy will only shallow copy and we
;; still reference the same underlying array
(defun copy-vec2 (vec)
  (gk:vec2 (gk:x vec) (gk:y vec)))

(defvar +empty-vec2+ (gk:vec2 0 0))

;; It's way too easy to use the same empty vector instance multiple places and
;; accidentally mutate it. This function makes sure we get a clean one each time.
(defun empty-vec2 ()
  "Creates an empty VEC2."
  (copy-vec2 +empty-vec2+))

(defun clamp-vec (vec min max)
  "Clamps the VEC2, VEC, between the MIN VEC2 and MAX VEC2."
  (gk:vec2 (clamp (gk:x vec) (gk:x min) (gk:x max))
           (clamp (gk:y vec) (gk:y min) (gk:y max))))

;; Disable copier to create a deep copy
(defstruct (rect (:copier nil))
  "Rectangle represented by bottom-left POS and SIZE"
  (pos (empty-vec2))
  (size (empty-vec2)))

(defun copy-rect (rect)
  (make-rect :pos (copy-vec2 (rect-pos rect))
             :size (copy-vec2 (rect-size rect))))

(defvar +empty-rect+
  (make-rect :pos (empty-vec2)
             :size (empty-vec2)))

(defun empty-rect ()
  (copy-rect +empty-rect+))

(defstruct (bbox (:copier nil))
  "Bounding Box (rectangle) represented by bottom-left BEG and top-right END"
  (beg (empty-vec2))
  (end (empty-vec2)))

(defun copy-bbox (bbox)
  (make-bbox :beg (copy-vec2 (bbox-beg bbox))
             :end (copy-vec2 (bbox-end bbox))))

(defvar +empty-bbox+
  (make-bbox :beg (empty-vec2)
             :end (empty-vec2)))

(defun empty-bbox ()
  (copy-bbox +empty-bbox+))

(defun rect->bbox (rect)
  (make-bbox :beg (rect-pos rect)
             :end (gk:add (rect-pos rect) (rect-size rect))))

(defun bbox->rect (bbox)
  (make-rect :pos (bbox-beg bbox)
             :size (gk:subt (bbox-end bbox) (bbox-beg bbox))))

(defmethod intersectsp ((a bbox) (b bbox))
  ;; https://silentmatt.com/rectangle-intersection/
  (let ((a1 (bbox-beg a))
        (a2 (bbox-end a))
        (b1 (bbox-beg b))
        (b2 (bbox-end b)))
    (and (< (gk:x a1) (gk:x b2))
         (> (gk:x a2) (gk:x b1))
         (< (gk:y a1) (gk:y b2))
         (> (gk:y a2) (gk:y b1)))))

(defmethod intersectsp ((a rect) (b rect))
  (intersectsp (rect->bbox a) (rect->bbox b)))

(defmethod intersectsp ((a rect) (b bbox))
  (intersectsp (rect->bbox a) b))

(defmethod intersectsp ((a bbox) (b rect))
  (intersectsp a (rect->bbox b)))

(defstruct background
  "Background. SPEED for parallax effect. SIZE is size of the background image.
Position isn't necessary as we tile the background image to fill the screen."
  (speed (empty-vec2))
  (size (empty-vec2)))

(defvar *white* (gk:vec4 1 1 1 1))

(defvar *random* (make-random-state t)
  "Random state")
(defvar *paused* nil
  "If T, ACT will not progress the game state.")
(defvar *gameover* nil
  "If T, the game is finished.")

(defvar *background* (make-background :speed (gk:vec2 0.25 0.0)
                                      :size (gk:vec2 256 256)))

(defvar *size* (gk:vec2 (* (gk:x (background-size *background*)) 2) (gk:y (background-size *background*)))
  "Size of the screen.")
(defvar *size/2* (gk:div *size* 2)
  "Midpoint of size.")

(defvar *bird* (make-rect :pos (empty-vec2)
                          :size (gk:vec2 16 16))
  "POS is the position in world coordinates. SIZE matches the image size.")

(defvar *pipe-width* 32
  "Width of a pipe. Matches the pipe image width.")

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
(defvar *flaksespeed* nil
  "Speed added to *POS* when the bird flaps its wings.")
(defvar *camera* nil
  "Position of camera. Things in world coordinates is translated to screen using this value.")
(defvar *pipes* nil
  "The pipes in the game.")
(defvar *level* nil
  "Current level. Used for difficulty and scoring.")
(defvar *level-complete* nil
  "T iff the level is currently completed.")
(defvar *score* nil
  "Current score.")

(defun world->screen (world &optional (camera *camera*))
  "Translates a world coordinate to screen coordinate."
  (gk:subt world camera))

(defun bird-collided-p ()
  "T iff the bird collides with any pipe or top/bottom of the screen."
  (let ((birdbox (rect->bbox *bird*)))
    ;; screen top
    (when (>= (gk:y (bbox-end birdbox)) (gk:y *size*))
      (return-from bird-collided-p t))
    ;; screen bottom
    (when (<= (gk:y (bbox-beg birdbox)) 0)
      (return-from bird-collided-p t))
    ;; pipes
    (do-each (p *pipes*)
      (when (intersectsp birdbox p)
        (return-from bird-collided-p t)))))

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
                                            *level-complete* nil
                                            *level* (+ *level* 1))
                                      (make-pipes))
                                    (toggle-pause)))
  (gk:bind-button :up :pressed (lambda () (flakse))))

;; FIXME: We draw UP TO *size* height, but don't record it. For the bottom pipe,
;; it might mean we collide above the pipe!
(defun draw-pipe (rect)
  "Draws a pipe at RECT. A full *SIZE* rect will be drawn, but displaced below
or above the screen."
  (let* ((rect (copy-rect rect))
         (free-y (- (gk:y *size*) (gk:y (rect-size rect))))
         (pos (rect-pos rect))
         (pos (gk:add pos (gk:vec2 0 (if (= (gk:y pos) 0) (negate free-y) free-y))))
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

(defmethod gk:draw ((background background))
  (let* ((speed (background-speed background))
         (size (background-size background))
         (start (floor (* (gk:x *camera*) (gk:x speed))))
         (diff (- (gk:x *camera*) start))
         (pages (floor (/ diff (gk:x size))))
         (x (+ start (* pages (gk:x size)))))
    (dotimes (i (+ (ceiling (/ (gk:x *size*) (gk:x size))) 1))
      (gk:draw-image (world->screen (gk:vec2 (+ x (* (gk:x size) i)) 0)) :background))))

(defmethod gk:draw ((app flaksefugl))
  ;; background
  (gk:draw *background*)
  ;; pipes
  (do-each (p *pipes*)
    (draw-pipe p))
  ;; bird
  (gk:draw-image (world->screen (rect-pos *bird*)) :bird :width (gk:x (rect-size *bird*)) :height (gk:x (rect-size *bird*)))
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

(defmethod gk:act ((app flaksefugl))
  (unless (or *gameover* *paused*)
    (setf *score* (+ *score* *level*)
          *speed* (gk:add *speed* *gravity*)
          (rect-pos *bird*) (gk:add (rect-pos *bird*) *speed*)
          (gk:x *camera*) (- (gk:x (rect-pos *bird*)) (gk:x *size/2*))
          *gameover* (bird-collided-p))
    (let ((last-pipe (aref *pipes* (- (array-dimension *pipes* 0) 1))))
      (when (> (gk:x (rect-pos *bird*)) (+ (gk:x (rect-pos last-pipe)) *pipe-width*))
        (toggle-pause)
        (setf *level-complete* t)))))


(defun make-sin-pipes ()
  (let* ((n 32)
         (s (/ pi n))
         (pipes (make-array n)))
    (do ((i 0 (+ i 1)))
        ((>= i n) pipes)
      (setf (aref pipes i) (make-rect :pos (gk:vec2 (+ (* i *pipe-width*) (gk:x *size/2*)) 0)
                                      :size (gk:vec2 *pipe-width* (* (gk:y *size/2*) (sin (* i s)))))))))
(defun make-pipes ()
  (setf *pipes* (make-sin-pipes)))

(defun reset ()
  "Reset game."
  (setf *gravity* (gk:vec2 0.0 -0.1)
        *speed* (gk:vec2 1.0 0.0)
        (rect-pos *bird*) *size/2*
        *flaksespeed* (gk:vec2 0.0 3.0)
        *camera* (gk:subt (rect-pos *bird*) *size/2*)
        *level* 1
        *level-complete* nil
        *score* 0
        *gameover* nil
        *pipes* nil)
  (make-pipes))

(defun start ()
  (gk:start 'flaksefugl))

(defun stop ()
  (gk:stop))
