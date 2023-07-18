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
            (:constructor %make-convex-hull ())
            (:include aabb)
            (:copier NIL)
            (:predicate NIL))
  (id 0 :type (unsigned-byte 32))
  (points (make-array 0 :adjustable T :fill-pointer T) :type vector)
  (triangles (make-array 0 :adjustable T :fill-pointer T) :type vector)
  (volume 0d0 :type double-float)
  (center (vec 0 0 0) :type vec3)
  ;; Private
  (aabb-p0 (vec 0 0 0) :type vec3)
  (aabb-p1 (vec 0 0 0) :type vec3)
  (diagonal 0.0 :type single-float))

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
            (:constructor plane (3d-vectors::%vx3 3d-vectors::%vy3 3d-vectors::%vz3 offset))
            (:copier NIL)
            (:predicate NIL))
  (offset 0.0 :type single-float))

(defun evalute-plane (plane p)
  (+ (plane-offset plane) (v. plane p)))

(defun make-plane (p0 p1 p2)
  (let ((v (vc (v- p1 p0) (v- p2 p0))))
    (plane (vx3 v) (vy3 v) (vz3 v) (- (v. p0 p0)))))

(defstruct (hull-face
            (:constructor hull-face (p0 p1 p2))
            (:copier NIL)
            (:predicate NIL))
  (p0 0 :type (unsigned-byte 32))
  (p1 0 :type (unsigned-byte 32))
  (p2 0 :type (unsigned-byte 32))
  (mark NIL :type boolean)
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

(defstruct (hull-vertex
            (:include vec3)
            (:constructor hull-vertex (&optional 3d-vectors::%vx3 3d-vectors::%vy3 3d-vectors::%vz3))
            (:copier NIL)
            (:predicate NIL))
  (mark NIL :type boolean))

(defstruct (aabb-node
            (:copier NIL)
            (:predicate NIL))
  (min (vec 0 0 0) :type vec3)
  (max (vec 0 0 0) :type vec3)
  (left NIL :type (or null aabb-node))
  (right NIL :type (or null aabb-node))
  (parent NIL :type (or null aabb-node))
  (count 0 :type fixnum)
  (indices (make-array 8 :element-type 'fixnum) :type (simple-array fixnum (8))))

(defstruct (normal-map
            (:constructor %make-normal-map ())
            (:copier NIL)
            (:predicate NIL))
  (normals (map-into (make-array 128) #'vec3) :type (simple-array vec3 (128)))
  (count 128 :type (unsigned-byte 8)))

(defun tessellate-triangle (map level p0 p1 p2 count)
  (cond ((< 0 level)
         (let ((p01 (nvunit (v+ p0 p1)))
               (p12 (nvunit (v+ p1 p2)))
               (p20 (nvunit (v+ p2 p0))))
           (setf count (tessellate-triangle map (1- level) p0 p01 p20 count))
           (setf count (tessellate-triangle map (1- level) p1 p12 p01 count))
           (setf count (tessellate-triangle map (1- level) p2 p20 p12 count))
           (setf count (tessellate-triangle map (1- level) p01 p12 p20 count))))
        (T
         (let ((plane (make-plane p0 p1 p2)))
           (nvunit plane)
           (setf (plane-offset plane) 0.0)
           (let ((index (bit-reversal count (length (normal-map-normals map)))))
             (setf (aref (normal-map-normals map) index) plane)
             (incf count))))))

(defun make-normal-map ()
  (let ((map (%make-normal-map)))
    (let ((p0 (vec +1 0 0)) (p1 (vec -1 0 0))
          (p2 (vec 0 +1 0)) (p3 (vec 0 -1 0))
          (p4 (vec 0 0 +1)) (p5 (vec 0 0 -1)))
      (let ((count 0) (subdivisions 2))
        (setf count (tessellate-triangle map subdivisions p4 p0 p2 count))
        (setf count (tessellate-triangle map subdivisions p0 p5 p2 count))
        (setf count (tessellate-triangle map subdivisions p5 p1 p2 count))
        (setf count (tessellate-triangle map subdivisions p1 p4 p2 count))
        (setf count (tessellate-triangle map subdivisions p0 p4 p3 count))
        (setf count (tessellate-triangle map subdivisions p5 p0 p3 count))
        (setf count (tessellate-triangle map subdivisions p1 p5 p3 count))
        (setf count (tessellate-triangle map subdivisions p4 p1 p3 count))))))

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

(defun tetrahedrum-volume (p0 p1 p2 p3)
  (v. (v- p3 p0) (vc (v- p1 p0) (v- p2 p0))))

(defun bit-reversal (v base)
  (let ((x 0)
        (power (truncate (log base 2))))
    (loop do (incf x (ash (logand v 1) power))
             (setf v (ash v -1))
             (decf power)
          while (/= 0 v))
    x))

;;; Actual algorithm

(defun make-convex-hull (vertex-cloud dist-to-l max-vertex-count)
  (let* ((hull (%make-convex-hull))
         ;;(tree-count (* 2 (max 4 (truncate (length vertex-cloud) (* 3 (ash 8 -1))))))
         (points (make-array (truncate (length vertex-cloud) 3))))
    (loop for i from 0 below (length points)
          for j from 0 by 3
          do (setf (aref points i) (hull-vertex (aref vertex-cloud (+ j 0))
                                                (aref vertex-cloud (+ j 1))
                                                (aref vertex-cloud (+ j 2)))))
    (let ((count (init-vertex-array hull points)))
      (when (<= 4 (length (convex-hull-points hull)))
        (calculate-convex-hull-3d hull points count dist-to-l max-vertex-count))
      hull)))

(defun unique-points (points)
  (setf points (sort points #'v<))
  (let ((index-count 0))
    (loop for i from 1 below (length points)
          do (loop while (< i (length points))
                   do (when (v/= (aref points index-count) (aref points i))
                        (incf index-count)
                        (setf (aref points index-count) (aref points i))
                        (loop-finish))))
    (adjust-array points (1+ index-count))))

(defun build-tree (points)
  (let ((points (unique-points points)))
    (build-tree-recurse NIL points 0 (length points) 0)))

(defun build-tree-recurse (parent points start end base)
  (let (tree
        (count (- end start))
        (min-p (vec +1.0e15 +1.0e15 +1.0e15))
        (max-p (vec -1.0e15 -1.0e15 -1.0e15)))
    (cond ((<= count 8)
           (let ((clump (make-aabb-node :count count)))
             (dotimes (i count)
               (setf (aref (aabb-node-indices clump) i) (+ i start base))
               (setf min-p (vmin min-p (aref points (+ i start))))
               (setf max-p (vmax max-p (aref points (+ i start)))))
             (setf tree clump)))
          (T
           (let ((median (vec3))
                 (varian (vec3)))
             (loop for i from start below end
                   for p = (aref points i)
                   do (setf min-p (vmin min-p p))
                      (setf max-p (vmax max-p p))
                      (nv+ median p)
                      (nv+ varian (v* p p)))
             (nv* varian count)
             (nv- varian (v* median median))
             (let ((dim #'vx3) (max-varian -1.0e10))
               (dolist (d (list #'vx3 #'vy3 #'vz3))
                 (when (< max-varian (funcall d varian))
                   (setf dim d)
                   (setf max-varian (funcall d varian))))
               (let ((test (funcall dim (v/ median count)))
                     (i0 start) (i1 (1- end)))
                 (loop do (loop while (and (<= i0 i1) (not (< test (funcall dim (aref points i0)))))
                                do (incf i0))
                          (loop while (and (<= i0 i1) (not (< (funcall dim (aref points i1)) test)))
                                do (decf i1))
                          (when (< i0 i1)
                            (rotatef (aref points i0) (aref points i1))
                            (incf i0) (decf i1))
                       while (<= i0 i1))
                 (when (or (= start i0) (<= (1- end) i0)) (setf i0 (+ start (truncate count 2))))
                 (let ((tree (make-aabb-node)))
                   (setf (aabb-node-left tree) (build-tree-recurse tree points start i0 base))
                   (setf (aabb-node-right tree) (build-tree-recurse tree points i0 end (+ (- i0 start) base)))))))))
    (setf (aabb-node-parent tree) parent)
    (setf (aabb-node-min tree) (v- min-p (vec3 1.0e-3 1.0e-3 1.0e-3)))
    (setf (aabb-node-max tree) (v+ max-p (vec3 1.0e-3 1.0e-3 1.0e-3)))
    tree))

(defun support-vertex (tree-node points dir-plane &optional remove-entry-p)
  (let ((aabb-projection (make-array 64 :element-type 'single-float))
        (stack-pool (make-array 64))
        (dir (vcopy dir-plane))
        (stack 0)
        (max-projection -1.0e20)
        (ix (< 0 (vx3 dir-plane)))
        (iy (< 0 (vy3 dir-plane)))
        (iz (< 0 (vz3 dir-plane)))
        index)
    (flet ((support-point (node)
             (vec (vx (if ix (aabb-node-max node) (aabb-node-min node)))
                  (vy (if iy (aabb-node-max node) (aabb-node-min node)))
                  (vz (if iz (aabb-node-max node) (aabb-node-min node)))))
           (push-stack (distance node)
             (setf (aref aabb-projection stack) distance)
             (setf (aref stack-pool stack) node)
             (incf stack)))
      (push-stack 1.0e20 tree-node)
      (loop while (< 0 stack)
            do (decf stack)
               (let ((box-support-value (aref aabb-projection stack))
                     (me (aref stack-pool stack)))
                 (when (< max-projection box-support-value)
                   (cond ((and (aabb-node-left me) (aabb-node-right me))
                          (let ((left-support-distance (v. (support-point (aabb-node-left me)) dir))
                                (right-support-distance (v. (support-point (aabb-node-right me)) dir)))
                            (cond ((<= left-support-distance right-support-distance)
                                   (push-stack left-support-distance (aabb-node-left me))
                                   (push-stack right-support-distance (aabb-node-right me)))
                                  (T
                                   (push-stack right-support-distance (aabb-node-right me))
                                   (push-stack left-support-distance (aabb-node-left me))))))
                         (T
                          (loop for i from 0 below (length (aabb-node-indices me))
                                for p = (aref points (aref (aabb-node-indices me) i))
                                do (cond ((not (hull-vertex-mark p))
                                          (let ((dist (v. p dir)))
                                            (when (< max-projection dist)
                                              (setf max-projection dist)
                                              (setf index (aref (aabb-node-indices me) i)))))
                                         (remove-entry-p
                                          (decf (aabb-node-count me))
                                          (setf (aref (aabb-node-indices me) i) (aref (aabb-node-indices me) (aabb-node-count me)))
                                          (decf i))))
                          (when (= 0 (aabb-node-count me))
                            (when (aabb-node-parent me)
                              (let* ((parent (aabb-node-parent me))
                                     (grandparent (aabb-node-parent parent))
                                     (sibling (if (eq me (aabb-node-left parent))
                                                  (aabb-node-right parent)
                                                  (aabb-node-left parent))))
                                (cond (grandparent
                                       (setf (aabb-node-parent sibling) grandparent)
                                       (if (eq parent (aabb-node-right grandparent))
                                           (setf (aabb-node-right grandparent) sibling)
                                           (setf (aabb-node-left grandparent) sibling)))
                                      (T
                                       (setf (aabb-node-parent sibling) NIL)
                                       (setf tree-node sibling))))))))))))
    index))

(defun init-vertex-array (hull points)
  (let* ((tree (build-tree points))
         (count (length points))
         (normal-map (make-normal-map))
         (m-points (convex-hull-points hull))
         (diag (vdistance (aabb-node-max tree) (aabb-node-min tree))))
    (adjust-array m-points count)
    (v<- (convex-hull-aabb-p0 hull) (aabb-node-min tree))
    (v<- (convex-hull-aabb-p1 hull) (aabb-node-max tree))
    (setf (convex-hull-diagonal hull) diag)
    (let ((index0 (support-vertex tree points (aref (normal-map-normals normal-map) 0)))
          (valid-tetrahedrum-p NIL)
          (e1 (vec3)) (e2 (vec3)) (e3 (vec3)) (normal (vec3)))
      (setf (aref m-points 0) (aref points index0))
      ;; FIXME: dunno if this property transferring here is OK.
      (setf (hull-vertex-mark (aref points index0)) T)
      (loop for normal across (normal-map-normals normal-map)
            for index = (support-vertex tree points normal)
            do (v<- e1 (aref points index))
               (nv- e1 (aref m-points 0))
               (when (< (* diag diag 1.0e-4) (vsqrlength e1))
                 (setf (aref m-points 1) (aref points index))
                 (setf (hull-vertex-mark (aref points index)) T)
                 (setf valid-tetrahedrum-p T)
                 (return)))
      (assert valid-tetrahedrum-p)
      (setf valid-tetrahedrum-p NIL)
      (loop for i from 2 below (normal-map-count normal-map)
            for index = (support-vertex tree points (aref (normal-map-normals normal-map) i))
            do (v<- e2 (aref points index))
               (nv- e2 (aref m-points 0))
               (v<- normal (vc e1 e2))
               (when (< (* diag diag 1.0e-4) (vlength normal))
                 (setf (aref m-points 2) (aref points index))
                 (setf (hull-vertex-mark (aref points index)) T)
                 (setf valid-tetrahedrum-p T)
                 (return)))
      ;; Find the largest possible tetrahedrum
      (assert valid-tetrahedrum-p)
      (setf valid-tetrahedrum-p NIL)
      (setf index0 (support-vertex tree points normal))
      (v<- e3 (aref points index0))
      (nv- e3 (aref m-points 0))
      (when (< (* diag diag 1.0e-6) (abs (v. normal e3)))
        (setf (aref m-points 3) (aref points index0))
        (setf (hull-vertex-mark (aref points index0)) T)
        (setf valid-tetrahedrum-p T))
      (unless valid-tetrahedrum-p
        (setf index0 (support-vertex tree points (v- normal)))
        (v<- e3 (aref points index0))
        (nv- e3 (aref m-points 0))
        (when (< (* diag diag 1.0e-6) (abs (v. normal e3)))
          (setf (aref m-points 3) (aref points index0))
          (setf (hull-vertex-mark (aref points index0)) T)
          (setf valid-tetrahedrum-p T)))
      (unless valid-tetrahedrum-p
        (loop for i from 3 below (normal-map-count normal-map)
              for index = (support-vertex tree points (aref (normal-map-normals normal-map) i))
              do (v<- e3 (aref points index))
                 (nv- e3 (aref m-points 0))
                 (when (< (* diag diag 1.0e-6) (abs (v. normal e3)))
                   (setf (aref m-points 3) (aref points index))
                   (setf (hull-vertex-mark (aref points index)) T)
                   (setf valid-tetrahedrum-p T)
                   (return))))
      (assert valid-tetrahedrum-p)
      (adjust-array m-points 4)
      (when (< 0 (tetrahedrum-volume (aref m-points 0) (aref m-points 1) (aref m-points 2) (aref m-points 3)))
        (rotatef (aref m-points 2) (aref m-points 3)))
      count)))

(defun calculate-convex-hull-3d (hull points count dist-to-l max-vertex-count)
  )
