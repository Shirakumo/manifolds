(in-package #:org.shirakumo.fraf.manifolds)

(defstruct (aabb
            (:constructor make-aabb (&optional min max))
            (:copier NIL)
            (:predicate NIL))
  (min (vec 0 0 0) :type vec3)
  (max (vec 0 0 0) :type vec3))

(defmethod print-object ((aabb aabb) stream)
  (print-unreadable-object (aabb stream :type T)
    (format stream "~a ~a" (aabb-min aabb) (aabb-max aabb))))

(defun copy-aabb (a)
  (make-aabb (vcopy (aabb-min a)) (vcopy (aabb-max a))))

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

(defun aabb-closest-point (a p)
  (vmin (vmax p (aabb-min a)) (aabb-max a)))

(defun aabb-longest-axis (a)
  (let ((x (- (vx (aabb-max a)) (vx (aabb-min a))))
        (y (- (vy (aabb-max a)) (vy (aabb-min a))))
        (z (- (vz (aabb-max a)) (vz (aabb-min a)))))
    (if (< x y)
        (if (< y z) 2 1)
        (if (< x z) 2 0))))

(defun aabb-bounds (vertices &optional (aabb (make-aabb)))
  (let ((min (aabb-min aabb))
        (max (aabb-max aabb)))
    (setf (vx min) (setf (vx max) (aref vertices 0)))
    (setf (vy min) (setf (vy max) (aref vertices 1)))
    (setf (vz min) (setf (vz max) (aref vertices 2)))
    (loop for i from 3 below (length vertices) by 3
          for x = (aref vertices (+ i 0))
          for y = (aref vertices (+ i 1))
          for z = (aref vertices (+ i 2))
          do (setf (vx min) (min (vx min) x))
             (setf (vy min) (min (vy min) y))
             (setf (vz min) (min (vz min) z))
             (setf (vx max) (max (vx max) x))
             (setf (vy max) (max (vy max) y))
             (setf (vz max) (max (vz max) z)))
    aabb))
