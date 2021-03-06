(declaim (optimize (speed 0) (space 0) (debug 3)))

(cl:in-package :flaksefugl)

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

(defstruct bird
  "The player. RECT is position of the bird in world coordinates and size of
bird image. SPEED is current velocity. MIN-SPEED is the maximum negative
velocity. MAX-SPEED is the maximum velocity. FLAKSESPEED is the speed added when
flapping wings."
  (rect (empty-rect))
  (speed (empty-vec2))
  (min-speed (empty-vec2))
  (max-speed (empty-vec2))
  (flaksespeed (empty-vec2)))

(defun default-bird ()
  (make-bird :rect (make-rect :pos *size/2*
                              :size (gk:vec2 16 16))
             :speed (gk:vec2 1.0 0.0)
             :min-speed (gk:vec2 -10 -10)
             :max-speed (gk:vec2 10 4)
             :flaksespeed (gk:vec2 0 3)))

(defun bird-pos (bird)
  (rect-pos (bird-rect bird)))

(defun (setf bird-pos) (pos bird)
  (setf (rect-pos (bird-rect bird)) pos))

(defun bird-x (bird)
  (gk:x (bird-pos bird)))

(defun bird-y (bird)
  (gk:y (bird-pos bird)))

(defun bird-size (bird)
  (rect-size (bird-rect bird)))

(defun bird-width (bird)
  (gk:x (bird-size bird)))

(defun bird-height (bird)
  (gk:y (bird-size bird)))

(defvar *bird* nil
  "The player.")

(defvar *pipe-width* 32
  "Width of a pipe. Matches the pipe image width.")

;; We set these in RESET to easily restart the game and support a more repl
;; driven development cycle
(defvar *gravity* nil
  "Speed added to bird-speed each tick.")
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
  (let ((birdbox (rect->bbox (bird-rect *bird*))))
    ;; screen top
    (when (>= (bbox-y2 birdbox) (gk:y *size*))
      (return-from bird-collided-p t))
    ;; screen bottom
    (when (<= (bbox-y1 birdbox) 0)
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

(defun flakse (&optional (bird *bird*))
  "Flap bird wings. Adds flaksespeed to speed while keeping it within
reasonable bounds."
  (setf (bird-speed bird) (clamp-vec (gk:add (bird-speed bird) (bird-flaksespeed bird))
                                     (bird-min-speed bird)
                                     (bird-max-speed bird))))

(defun toggle-pause ()
  "Toggle *PAUSED*."
  (setf *paused* (not *paused*)))

(defun next-level ()
  (setf (bird-speed *bird*) (gk:add (bird-speed *bird*) (gk:vec2 0.25 0))
        *level-complete* nil
        *level* (+ *level* 1))
  (make-pipes))

(defmethod gk:post-initialize ((app flaksefugl))
  (reset)
  (gk:bind-button :enter :pressed (lambda () (reset)))
  (gk:bind-button :space :pressed (lambda ()
                                    (when *level-complete*
                                      (next-level))
                                    (toggle-pause)))
  (gk:bind-button :up :pressed (lambda () (flakse))))

;; FIXME: We draw UP TO *size* height, but don't record it. For the bottom pipe,
;; it might mean we collide above the pipe!
(defun draw-pipe (rect)
  "Draws a pipe at RECT. A full *SIZE* rect will be drawn, but displaced below
or above the screen."
  (let* ((rect (deep-copy rect))
         (free-y (- (gk:y *size*) (rect-height rect)))
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

(defmethod gk:draw ((bird bird))
  (gk:draw-image (world->screen (bird-pos *bird*)) :bird :width (bird-width *bird*) :height (bird-height *bird*)))

(defmethod gk:draw ((app flaksefugl))
  ;; background
  (gk:draw *background*)
  ;; pipes
  (do-each (p *pipes*)
    (draw-pipe p))
  ;; bird
  (gk:draw *bird*)
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
          (bird-speed *bird*) (gk:add (bird-speed *bird*) *gravity*)
          (bird-pos *bird*) (gk:add (bird-pos *bird*) (bird-speed *bird*))
          (gk:x *camera*) (- (bird-x *bird*) (gk:x *size/2*))
          *gameover* (bird-collided-p))
    (let ((last-pipe (aref *pipes* (- (array-dimension *pipes* 0) 1))))
      (when (> (bird-x *bird*) (+ (rect-x last-pipe) *pipe-width*))
        (toggle-pause)
        (setf *level-complete* t)))))


(defun make-sin-pipes ()
  (let* ((n 32)
         (s (/ pi n))
         (pipes (make-array n)))
    (do ((i 0 (+ i 1)))
        ((>= i n) pipes)
      (setf (aref pipes i) (make-rect :pos (gk:vec2 (+ (* i *pipe-width*) (bird-x *bird*)) 0)
                                      :size (gk:vec2 *pipe-width* (* (gk:y *size/2*) (sin (* i s)))))))))
(defun make-pipes ()
  (setf *pipes* (make-sin-pipes)))

(defun reset ()
  "Reset game."
  (setf *gravity* (gk:vec2 0.0 -0.1)
        *bird* (default-bird)
        *camera* (gk:subt (bird-pos *bird*) *size/2*)
        *level* 0
        *level-complete* nil
        *score* 0
        *gameover* nil
        *pipes* nil)
  (next-level))

(defun start ()
  (gk:start 'flaksefugl))

(defun stop ()
  (gk:stop))
