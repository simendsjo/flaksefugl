(cl:in-package :flaksefugl)

;; Thanks to TheGreatCatAdorer#3666
;; Ref https://discord.com/channels/297478281278652417/973665360706424882/973696910928015370
(defgeneric deep-copy (value)
  (:documentation "Deep copies VALUE so that modification to either original or
  result doesn't affect the other.")
  (:method (value)
    value)
  (:method ((value vector))
    (map 'vector #'deep-copy vector)))

(defun negate (x)
  "Negate X."
  (* x -1))

(defvar +empty-vec2+ (gk:vec2 0 0))

;; It's way too easy to use the same empty vector instance multiple places and
;; accidentally mutate it. This function makes sure we get a clean one each time.
(defun empty-vec2 ()
  "Creates an empty VEC2."
  (deep-copy +empty-vec2+))

(defun clamp-vec (vec min max)
  "Clamps the VEC2, VEC, between the MIN VEC2 and MAX VEC2."
  (gk:vec2 (clamp (gk:x vec) (gk:x min) (gk:x max))
           (clamp (gk:y vec) (gk:y min) (gk:y max))))

;; Disable copier to create a deep copy
(defstruct (rect (:copier nil))
  "Rectangle represented by bottom-left POS and SIZE"
  (pos (empty-vec2))
  (size (empty-vec2)))

(defun rect-x (rect)
  (gk:x (rect-pos rect)))

(defun (setf rect-x) (x rect)
  (setf (gk:x (rect-pos rect)) x))

(defun rect-y (rect)
  (gk:y (rect-pos rect)))

(defun (setf rect-y) (y rect)
  (setf (gk:y (rect-pos rect)) y))

(defun rect-width (rect)
  (gk:x (rect-size rect)))

(defun (setf rect-width) (width rect)
  (setf (gk:x (rect-size rect)) rect))

(defun rect-height (rect)
  (gk:y (rect-size rect)))

(defun (setf rect-height) (heigth rect)
  (setf (gk:y (rect-size rect)) rect))

(defmethod deep-copy ((rect rect))
  (make-rect :pos (deep-copy (rect-pos rect))
             :size (deep-copy (rect-size rect))))

(defvar +empty-rect+
  (make-rect :pos (empty-vec2)
             :size (empty-vec2)))

(defun empty-rect ()
  (deep-copy +empty-rect+))

(defstruct (bbox (:copier nil))
  "Bounding Box (rectangle) represented by bottom-left BEG and top-right END"
  (beg (empty-vec2))
  (end (empty-vec2)))

(defun bbox-x1 (bbox)
  (gk:x (bbox-beg bbox)))

(defun bbox-y1 (bbox)
  (gk:y (bbox-beg bbox)))

(defun bbox-x2 (bbox)
  (gk:x (bbox-end bbox)))

(defun bbox-y2 (bbox)
  (gk:y (bbox-end bbox)))

(defmethod deep-copy ((bbox bbox))
  (make-bbox :beg (deep-copy (bbox-beg bbox))
             :end (deep-copy (bbox-end bbox))))

(defvar +empty-bbox+
  (make-bbox :beg (empty-vec2)
             :end (empty-vec2)))

(defun empty-bbox ()
  (deep-copy +empty-bbox+))

(defun rect->bbox (rect)
  (make-bbox :beg (rect-pos rect)
             :end (gk:add (rect-pos rect) (rect-size rect))))

(defun bbox->rect (bbox)
  (make-rect :pos (bbox-beg bbox)
             :size (gk:subt (bbox-end bbox) (bbox-beg bbox))))

(defgeneric intersectsp (a b)
  (:documentation "T iff the A and B overlaps."))

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
