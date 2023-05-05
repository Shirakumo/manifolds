#|
 This file is a part of manifolds
 (c) 2023 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.manifolds)

(defstruct (octtree-node
            (:constructor %octtree-node (location bsize faces &optional children)))
  (location NIL :type vec3)
  (bsize NIL :type vec3)
  (faces NIL :type (simple-array (unsigned-byte 32)))
  (children NIL :type (or null (simple-array octtree-node (8))))
  (neighbors (make-array 12) :type (simple-vector 12)))

(defun face-in-volume-p (v0 v1 v2 c e)
  (declare (type vec3 v0 v1 v2 c e))
  ;; SAT
  (let* ((v0 (v- v0 c)) (v1 (v- v1 c)) (v2 (v- v2 c))
         (f0 (v- v1 v0)) (f1 (v- v2 v1)) (f2 (v- v0 v2)))
    (flet ((test-axis (ax ay az r)
             (let ((p0 (+ (* (vx v0) ax) (* (vy v0) ay) (* (vz v0) az)))
                   (p1 (+ (* (vx v1) ax) (* (vy v1) ay) (* (vz v1) az)))
                   (p2 (+ (* (vx v2) ax) (* (vy v2) ay) (* (vz v2) az))))
               (< r (max (- (max p0 p1 p2)) (min p0 p1 p2))))))
      (declare (inline test-axis))
      (not (or (test-axis 0 (- (vz f0)) (+ (vy f0)) (+ (* (vy e) (abs (vz f0))) (* (vz e) (abs (vy f0)))))
               (test-axis 0 (- (vz f1)) (+ (vy f1)) (+ (* (vy e) (abs (vz f1))) (* (vz e) (abs (vy f1)))))
               (test-axis 0 (- (vz f2)) (+ (vy f2)) (+ (* (vy e) (abs (vz f2))) (* (vz e) (abs (vy f2)))))
               (test-axis (+ (vz f0)) 0 (- (vx f0)) (+ (* (vx e) (abs (vz f0))) (* (vz e) (abs (vx f0)))))
               (test-axis (+ (vz f1)) 0 (- (vx f1)) (+ (* (vx e) (abs (vz f1))) (* (vz e) (abs (vx f1)))))
               (test-axis (+ (vz f2)) 0 (- (vx f2)) (+ (* (vx e) (abs (vz f2))) (* (vz e) (abs (vx f2)))))
               (test-axis (- (vy f0)) (+ (vx f0)) 0 (+ (* (vx e) (abs (vy f0))) (* (vy e) (abs (vx f0)))))
               (test-axis (- (vy f1)) (+ (vx f1)) 0 (+ (* (vx e) (abs (vy f1))) (* (vy e) (abs (vx f1)))))
               (test-axis (- (vy f2)) (+ (vx f2)) 0 (+ (* (vx e) (abs (vy f2))) (* (vy e) (abs (vx f2)))))
               (< (vx e) (min (vx v0) (vx v1) (vx v2)))
               (< (vy e) (min (vy v0) (vy v1) (vy v2)))
               (or (< (max (vz v0) (vz v1) (vz v2)) (- (vz e)))
                   (< (vz e) (min (vz v0) (vz v1) (vz v2))))
               (let ((n (vc f0 f1)))
                 (< (+ (* (vx e) (abs (vx n)))
                       (* (vy e) (abs (vy n)))
                       (* (vz e) (abs (vz n))))
                    (v. n v0))))))))

(defun faces-in-volume (vertices faces location bsize)
  (let ((new-faces (make-array 0 :element-type '(unsigned-byte 32) :adjustable T :fill-pointer T)))
    (do-faces (a b c faces (coerce new-faces '(simple-array (unsigned-byte 32))))
      (when (face-in-volume-p a b c location bsize)
        (vector-push-extend a new-faces)
        (vector-push-extend b new-faces)
        (vector-push-extend c new-faces)))))

(defun build-octtree (vertices faces location bsize)
  (cond ((= 0 (length faces))
         (%octtree-node location bsize faces))
        (T
         (let ((subsize (v* bsize 0.5))
               (children (make-array 8)))
           (loop for off across #(#.(vec +1 +1 +1)
                                  #.(vec -1 +1 +1)
                                  #.(vec +1 -1 +1)
                                  #.(vec +1 +1 -1)
                                  #.(vec -1 -1 +1)
                                  #.(vec +1 -1 -1)
                                  #.(vec -1 +1 -1)
                                  #.(vec -1 -1 -1))
                 for subloc = (v+ location (v* off subsize))
                 for subfaces = (faces-in-volume vertices faces subloc subsize)
                 for i from 0
                 do (setf (aref children i) (build-octtree vertices subfaces subloc subsize)))
           (%octtree-node location bsize faces children)))))

(defun build-octtree-neighborhood (node)
  (let ((children (octtree-node-children node)))
    (when children
      (map NIL #'build-octtree-neighborhood children)
      (labels ((connect-nodes-x (l r)
                 (when (or (octtree-node-children l)
                           (octtree-node-children r))
                   (setf (aref (octtree-node-neighbors l) 0) r)
                   (setf (aref (octtree-node-neighbors r) 3) l)
                   (loop for i from 0 below 4
                         do (connect-nodes-x (aref (octtree-node-children l) (+ 4 i))
                                             (aref (octtree-node-children r) (+ 0 i))))))
               (connect-nodes-y (l r)
                 (when (or (octtree-node-children l)
                           (octtree-node-children r))
                   (setf (aref (octtree-node-neighbors l) 1) r)
                   (setf (aref (octtree-node-neighbors r) 4) l)
                   (loop for i across #(0 1 4 5)
                         do (connect-nodes-x (aref (octtree-node-children l) (+ 2 i))
                                             (aref (octtree-node-children r) (+ 0 i))))))
               (connect-nodes-z (l r)
                 (when (or (octtree-node-children l)
                           (octtree-node-children r))
                   (setf (aref (octtree-node-neighbors l) 2) r)
                   (setf (aref (octtree-node-neighbors r) 5) l)
                   (loop for i from 0 below 4
                         do (connect-nodes-x (aref (octtree-node-children l) (+ 1 (* i 2)))
                                             (aref (octtree-node-children r) (+ 0 (* i 2))))))))
        (loop for x from 0 below 4
              for z = (* 2 x)
              for y across #(0 1 4 5)
              do (connect-nodes-x (aref children (+ 0 x)) (aref children (+ 4 x)))
                 (connect-nodes-y (aref children (+ 0 y)) (aref children (+ 2 y)))
                 (connect-nodes-z (aref children (+ 0 z)) (aref children (+ 1 z))))))))

#++
(defun build-octtree-empty-neighborhood (node)
  (let ((children (octtree-node-children node)))
    (when children
      (map NIL #'build-octtree-empty-neighborhood children)
      (labels ((connect-nodes-x (l r))
               (connect-nodes-y (l r))
               (connect-nodes-z (l r)))
        (loop for x from 0 below 4
              for z = (* 2 x)
              for y across #(0 1 4 5)
              do (connect-nodes-x (aref children (+ 0 x)) (aref children (+ 4 x)))
                 (connect-nodes-y (aref children (+ 0 y)) (aref children (+ 2 y)))
                 (connect-nodes-z (aref children (+ 0 z)) (aref children (+ 1 z))))))))
