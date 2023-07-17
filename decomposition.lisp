;;;; This is a port of the V-HACD implementation by Khaled Mamou et al.
;;;; currently (2023) accessible at https://github.com/kmammou/v-hacd
;;;; With the following differences:
;;;; 
;;;;   - We use libraries instead of bespoke implementations:
;;;;     - 3d-spaces (for the kd-tree)
;;;;     - 3d-vectors
;;;;     - quickhull
;;;;   - The port of parallel code is omitted
;;;;   - We only implement the "flood fill" mode, omitting the surface
;;;;     and raycast implementations. You should instead use the
;;;;     conversion methods in this library to obtain a proper
;;;;     closed 2-manifold if your mesh should be degenerate.
;;;; 
;;;; Any other divergence is purely due to differences in aesthetics and
;;;; conventions between C++ and Lisp. If this port should exhibit bugs
;;;; that are not present in the original code, please file a report.
;;;; 

(in-package #:org.shirakumo.fraf.manifolds)

;;;; Support structures
;;; NOTE: Triangles and Vertices are implicitly represented by an index into an array.

(defstruct (aabb
            (:constructor make-aabb (&optional min max))
            (:copier NIL)
            (:predicate NIL))
  (min (vec 0 0 0) :type vec3)
  (max (vec 0 0 0) :type vec3))

(defun aabb-union (a b)
  (make-aabb (vmin (aabb-min a) (aabb-min b))
             (vmax (aabb-max a) (aabb-max b))))

(defun aabb-intersects-p (a b)
  (and (not (or (< (vx3 (aabb-max b)) (vx3 (aabb-min a)))
                (< (vx3 (aabb-max a)) (vx3 (aabb-min b)))))
       (not (or (< (vy3 (aabb-max b)) (vy3 (aabb-min a)))
                (< (vy3 (aabb-max a)) (vy3 (aabb-min b)))))
       (not (or (< (vz3 (aabb-max b)) (vz3 (aabb-min a)))
                (< (vz3 (aabb-max a)) (vz3 (aabb-min b)))))))

(defun aabb-surface-area (a)
  (let ((dx (- (vx3 (aabb-max a)) (vx3 (aabb-min a))))
        (dy (- (vy3 (aabb-max a)) (vy3 (aabb-min a))))
        (dz (- (vz3 (aabb-max a)) (vz3 (aabb-min a)))))
    (* 2.0 (+ (* dx dy) (* dx dz) (* dy dz)))))

(defun aabb-volume (a)
  (let ((dx (- (vx3 (aabb-max a)) (vx3 (aabb-min a))))
        (dy (- (vy3 (aabb-max a)) (vy3 (aabb-min a))))
        (dz (- (vz3 (aabb-max a)) (vz3 (aabb-min a)))))
    (* dx dy dz)))

(defun aabb-inflate (a ratio)
  (let ((inflate (* (v2norm (v- (aabb-min a) (aabb-max a))) 0.5 ratio)))
    (make-aabb (v- (aabb-min a) inflate) (v+ (aabb-max a) inflate))))

(defun aabb-size (a)
  (v- (aabb-max a) (aabb-min a)))

(defun aabb-center (a)
  (nv* (v+ (aabb-max a) (aabb-min a)) 0.5))

(defstruct (convex-hull
            (:constructor %make-convex-hull (min max points triangles volume center))
            (:include aabb)
            (:copier NIL)
            (:predicate NIL))
  (id 0 :type (unsigned-byte 32))
  (points NIL :type (simple-array single-float))
  (triangles NIL :type (simple-array (unsigned-byte 32)))
  (volume 0d0 :type double-float)
  (center NIL :type vec3))

(defstruct (ch-parameters
            (:copier NIL)
            (:predicate NIL))
  (convex-hulls 64 :type (unsigned-byte 32))
  (resolution 400000 :type (unsigned-byte 32))
  (error-percentage 1.0 :type single-float)
  (max-recursion-depth 10 :type (unsigned-byte 32))
  (shrink-wrap T :type boolean)
  (max-vertices-per-convex-hull 64 :type (unsigned-byte 32))
  (minimum-edge-length 2 :type (unsigned-byte 32)))

;;; Skip Googol implementation

(defstruct (plane
            (:include vec3)
            (:constructor plane ())
            (:copier NIL)
            (:predicate NIL))
  (offset 0.0 :type single-float))

(defun evalute-plane (plane p)
  (+ (plane-offset plane) (v. plane p)))

(defstruct (hull-face
            (:constructor hull-face (p0 p1 p2))
            (:copier NIL)
            (:predicate NIL))
  (p0 0 :type (unsigned-byte 32))
  (p1 0 :type (unsigned-byte 32))
  (p2 0 :type (unsigned-byte 32))
  (mark 0 :type (unsigned-byte 32))
  (twin () :type list))

(defun evaluate-hull-face (face vertices point)
  (let ((p0 (v vertices (hull-face-p0 face)))
        (p1 (v vertices (hull-face-p1 face)))
        (p2 (v vertices (hull-face-p2 face))))
    (multiple-value-bind (det error) (determinant3x3 (v- p2 p0) (v- p1 p0) (v- point p0))
      (let* ((precision (/ 1.0d0 (ash 1 24)))
             (errbound (* error precision)))
        (if (< errbound (abs det))
            det
            (error "Not implemented"))))))

(defun hull-face-plane (face vertices)
  (let ((a (v vertices (hull-face-p0 face)))
        (b (v vertices (hull-face-p1 face)))
        (c (v vertices (hull-face-p2 face)))
        (plane (plane)))
    (v<- plane (vc (v- b a) (v- c a)))
    (setf (plane-offset plane) (- (v. plane a)))
    (let ((mag2 (v. plane plane)))
      (when (< 1.0e-16 mag2)
        (let ((invmag (/ (sqrt mag2))))
          (nv* plane invmag)
          (setf (plane-offset plane) (* (plane-offset plane) invmag)))))
    plane))

;;;; Additional ops
;;; They implement strange comparators.
(defun v<* (a b)
  (cond ((/= (vx3 a) (vx3 b))
         (< (vx3 a) (vx3 b)))
        ((/= (vy3 a) (vy3 b))
         (< (vy3 a) (vy3 b)))
        (T
         (< (vz3 a) (vz3 b)))))

(defun v>* (a b)
  (cond ((/= (vx3 a) (vx3 b))
         (< (vx3 a) (vx3 b)))
        ((/= (vy3 a) (vy3 b))
         (< (vy3 a) (vy3 b)))
        (T
         (< (vz3 a) (vz3 b)))))

(defun vmincoeff (a)
  (min (vx3 a) (vy3 a) (vz3 a)))

(defun vmaxcoeff (a)
  (max (vx3 a) (vy3 a) (vz3 a)))

(defun determinant3x3 (a b c)
  (let ((a01xa12 (* (vy3 a) (vz3 b)))
        (a02xa11 (* (vz3 a) (vy3 b)))
        (a00xa12 (* (vx3 a) (vz3 b)))
        (a02xa10 (* (vz3 a) (vx3 b)))
        (a00xa11 (* (vx3 a) (vy3 b)))
        (a01xa10 (* (vy3 a) (vx3 b))))
    (values (+ (* (- a01xa12 a02xa11) (vx3 c))
               (- (* (- a00xa12 a02xa10) (vy3 c)))
               (* (- a00xa11 a01xa10) (vz3 c)))
            (+ (+ (abs a01xa12) (* (abs a02xa11) (abs (vX3 c))))
               (+ (abs a00xa12) (* (abs a02xa10) (abs (vY3 c))))
               (+ (abs a00xa11) (* (abs a01xa10) (abs (vZ3 c))))))))

(defun mesh-volume (vertices faces)
  (let ((volume 0d0))
    (do-faces (a b c faces (abs (* volume (/ 1 6))))
      (incf volume (determinant3x3 (v vertices a) (v vertices b) (v vertices c))))))

(defun bit-reversal (v base)
  (let ((x 0)
        (power (truncate (log base 2))))
    (loop do (incf x (ash (logand v 1) power))
             (setf v (ash v -1))
             (decf power)
          while (/= 0 v))
    x))
