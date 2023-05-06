#|
 This file is a part of manifolds
 (c) 2023 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.manifolds)

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

(defun bounding-box (vertices)
  (let ((min (vec (aref vertices 0) (aref vertices 1) (aref vertices 2)))
        (max (vec (aref vertices 0) (aref vertices 1) (aref vertices 2))))
    (loop for i from 0 below (length vertices) by 3
          for x = (aref vertices (+ 0 i))
          for y = (aref vertices (+ 1 i))
          for z = (aref vertices (+ 2 i))
          do (setf (vx min) (min (vx min) x))
             (setf (vy min) (min (vy min) y))
             (setf (vz min) (min (vz min) z))
             (setf (vx max) (max (vx max) x))
             (setf (vy max) (max (vy max) y))
             (setf (vz max) (max (vz max) z)))
    (let ((bsize (nv* (v- max min) 0.5)))
      (values (nv+ min bsize) bsize))))

(defstruct (node
            (:constructor %node (location bsize faces &key number level)))
  (location NIL :type vec3)
  (bsize NIL :type vec3)
  (level 0 :type (unsigned-byte 32))
  (number 1 :type (unsigned-byte 32)) ; Number of occupied nodes in this branch
  (faces NIL :type (simple-array (unsigned-byte 32) (*)))
  (children NIL :type (or null (simple-array node (8))))
  (neighbors (make-array 6) :type (simple-vector 6))
  (empty-neighbors (make-array 6) :type (simple-vector 6)))

(defmethod print-object ((node node) stream)
  (print-unreadable-object (node stream :type T)
    (format stream "~a/~d [~d occupied] ~d faces"
            (node-location node) (node-level node) (node-number node)
            (length (node-faces node)))))

;; The children are oriented as follows:
;; 0 => x- y- z-
;; 1 => x- y- z+
;; 2 => x- y+ z-
;; 3 => x- y+ z+
;; 4 => x+ y- z-
;; 5 => x+ y- z+
;; 6 => x+ y+ z-
;; 7 => x+ y+ z+
;; The neighbors as follows:
;; 0 => x+
;; 1 => y+
;; 2 => z+
;; 3 => x-
;; 4 => y-
;; 5 => z-

(defun node-occupied-p (node)
  (< 0 (length (node-faces node))))

;; NOTE: what the fuck? This property is computed but for all I can tell it's just the same as occupied-p???
(defun node-exterior-p (node)
  (not (node-occupied-p node)))

(defun exterior-p (node point)
  (let ((location (node-location node))
        (bsize (node-bsize node)))
    (cond ((or (not (<= (- (vx location) (vx bsize)) (vx point) (+ (vx location) (vx bsize))))
               (not (<= (- (vy location) (vy bsize)) (vy point) (+ (vy location) (vy bsize))))
               (not (<= (- (vz location) (vz bsize)) (vz point) (+ (vz location) (vz bsize)))))
           T) ;; Point lies outside the node entirely.
          ((not (node-occupied-p node))
           (node-exterior-p node))
          ((= 0 (node-level node))
           NIL)
          (T
           (let ((i (+ (* 1 (< (vz point) (vz location)))
                       (* 2 (< (vy point) (vy location)))
                       (* 4 (< (vx point) (vx location))))))
             (exterior-p (aref (node-children node) i) point))))))

(defun build-octtree (vertices faces &key (resolution 1000))
  (multiple-value-bind (location bsize) (bounding-box vertices)
    (let ((node (%node location bsize faces)))
      (loop while (< (node-number node) resolution)
            do (split-octtree node vertices))
      (build-neighborhood node))))

(defun split-octtree (node vertices)
  (incf (node-level node))
  (setf (node-number node) 0)
  (cond ((= 1 (node-level node))
         (let ((bsize (v* (node-bsize node) 0.5))
               (children (make-array 8)))
           (setf (node-children node) children)
           (loop for i from 0 below 8
                 for location = (nv+ (nv* (vec (1- (* (ldb (byte 1 2) i) 2))
                                               (1- (* (ldb (byte 1 1) i) 2))
                                               (1- (* (ldb (byte 1 0) i) 2)))
                                          bsize)
                                     (node-location node))
                 for child-faces = (faces-in-volume vertices faces location bsize)
                 for child = (%node location bsize child-faces)
                 do (when (< 0 (length child-faces))
                      (setf (node-number child) 1)
                      (incf (node-number node)))
                    (setf (aref children i) child))))
        (T
         (loop for child across (node-children node)
               do (when (node-occupied-p child)
                    (split-octtree child vertices)
                    (incf (node-number node) (node-number child))))))
  node)

(defun build-neighborhood (node)
  (when (< 0 (node-level node))
    (mapc #'build-neighborhood (node-children node))
    (labels ((recur-full (l r li ri order)
               (setf (aref (node-neighbors l) ri) r)
               (setf (aref (node-neighbors r) li) l)
               (when (and (node-children l) (node-children r))
                 (loop for (j . i) across order
                       do (recur (aref (node-children l) i)
                                 (aref (node-children r) j)
                                 li ri order))))
             (recur-empty (l r li ri order)
               (cond ((and (node-occupied-p l) (node-occupied-p r))
                      (unless (= 0 (node-level l))
                        (loop for (j . i) across order
                              do (recur-empty (aref (node-children l) i)
                                              (aref (node-children r) j)
                                              li ri order))))
                     ((not (node-occupied-p l))
                      (setf (aref (node-empty-neighbors r) ri) l)
                      (loop for (j . i) across order
                            do (recur-empty l (aref (node-children r) j)
                                            li ri order)))
                     ((not (node-occupied-p r))
                      (setf (aref (node-empty-neighbors l) li) r)
                      (loop for (j . i) across order
                            do (recur-empty (aref (node-children l) i) r
                                            li ri order)))))
             (recur (l r li ri order)
               (recur-full l r li ri order)
               (recur-empty l r li ri order)))
      (let ((xo #((0 . 4) (1 . 5) (2 . 6) (3 . 7)))
            (yo #((0 . 2) (1 . 3) (4 . 6) (5 . 7)))
            (zo #((0 . 1) (2 . 3) (4 . 5) (6 . 7))))
        (loop for face from 0 below 6
              for (xi . xj) across xo
              for (yi . yj) across yo
              for (zi . zj) across zo
              do (recur (aref (node-children node) xi) (aref (node-children node) xj) 0 3 xo)
                 (recur (aref (node-children node) yi) (aref (node-children node) yj) 1 4 yo)
                 (recur (aref (node-children node) zi) (aref (node-children node) zj) 2 5 zo))))))

(defstruct (face
            (:constructor face (a b c)))
  (a 0 :type (unsigned-byte 32))
  (b 0 :type (unsigned-byte 32))
  (c 0 :type (unsigned-byte 32)))

(defun face= (a b)
  (and (= (face-a a) (face-a b))
       (= (face-b a) (face-b b))
       (= (face-c a) (face-c b))))

(defun construct-quad-manifold (tree)
  (let ((vcolor (make-hash-table :test 'equalp))
        (vertices (make-array 0 :element-type 'single-float :adjustable T :fill-pointer T))
        (quad-faces (make-array 0 :element-type '(unsigned-byte 32) :adjustable T :fill-pointer T))
        (vertex-faces (make-array 0 :adjustable T :fill-pointer T :initial-element ()))
        (offsets #2A((#.(vec 1 0 0) #.(vec 1 0 1) #.(vec 1 1 1) #.(vec 1 1 0))
                     (#.(vec 0 1 0) #.(vec 1 1 0) #.(vec 1 1 1) #.(vec 0 1 1))
                     (#.(vec 0 0 1) #.(vec 0 1 1) #.(vec 1 1 1) #.(vec 1 0 1))
                     (#.(vec 0 0 0) #.(vec 0 1 0) #.(vec 0 1 1) #.(vec 0 0 1))
                     (#.(vec 0 0 0) #.(vec 0 0 1) #.(vec 1 0 1) #.(vec 1 0 0))
                     (#.(vec 0 0 0) #.(vec 1 0 0) #.(vec 1 1 0) #.(vec 0 1 0)))))
    (labels ((make-face (node start offset)
               (let* ((vid (v* (v+ start offset) 2))
                      (id (gethash vid vcolor))
                      (nfaces (node-faces node)))
                 (unless id
                   (let ((d (v+ (v- (node-location node) (node-bsize node))
                                (v* offset 2 (node-bsize node)))))
                     (setf id (floor (length vertices) 3))
                     (setf (gethash vid vcolor) id)
                     (vector-push-extend (vx d) vertices)
                     (vector-push-extend (vy d) vertices)
                     (vector-push-extend (vz d) vertices)
                     (vector-push-extend () vertex-faces)))
                 (vector-push-extend id quad-faces)
                 (loop for i from 0 below (length node) by 3
                       do (pushnew (face (aref nfaces (+ 0 i)) (aref nfaces (+ 1 i)) (aref nfaces (+ 2 i)))
                                   (aref vertex-faces id) :test #'face=))))
             (recurse-face (node start)
               (cond ((= 0 (node-level node))
                      (when (node-occupied-p node)
                        (loop for i from 0 below 6
                              for empty across (node-empty-neighbors node)
                              do (when (and empty (node-exterior-p empty))
                                   (dotimes (j 4)
                                     (make-face node start (aref offsets i j)))))))
                     (T
                      (loop for i from 0
                            for child across (node-children node)
                            do (when (node-occupied-p child)
                                 (recurse-face child (v+ (v* start 2)
                                                           (vec (ldb (byte 1 2) i)
                                                                (ldb (byte 1 1) i)
                                                                (ldb (byte 1 0) i))))))))))
      (recurse-face tree (vec 0 0 0)))
    (values vcolor vertices quad-faces vertex-faces)))

(defun v (vertices i)
  (let ((i (* 3 i)))
    (vec (aref vertices (+ 0 i))
         (aref vertices (+ 1 i))
         (aref vertices (+ 2 i)))))

(defun construct-triangle-mesh (vcolor vertices quad-faces vertex-faces)
  )

(defun project-manifold (vertices faces orig-vertices orig-faces &key (iterations 20))
  (let* ((vertex-count (truncate (length vertices) 3))
         (len (min (vdistance (aref vertices (aref faces 0)) (aref vertices (aref faces 1)))
                   (vdistance (aref vertices (aref faces 0)) (aref vertices (aref faces 2)))))
         (colors (make-array (length vertices) :element-type 'bit :initial-element 1))
         (vertex-faces (vertex-faces faces (make-array vertex-count)))
         (face-normals (make-array (truncate (length faces) 3)))
         (invalid-vertices (make-array 0 :element-type '(unsigned-byte 32) :adjustable T :fill-pointer T))
         (invalid-indices (make-array (length vertex-faces) :element-type '(signed-byte 32) :initial-element -1))
         (visited (make-array (length vertex-faces) :element-type 'bit))
         (min-step (/ 2.0 iterations)))
    (labels ((convex-p (vertex normal)
               (loop for face across (aref vertex-faces vertex)
                     do (dotimes (i 3)
                          (when (< 0 (v. (v- (v vertices (aref faces (+ i (* 3 face))))
                                             (v vertices vertex))
                                         normal))
                            (return-from convex-p NIL)))
                     finally (return T))))
      (dotimes (iter iterations)
        (face-normals vertices faces face-normals)
        (dotimes (vertex vertex-count)
          (when (= 0 (sbit visited vertex))
            (let* ((closest (find-closest vertex))
                   (move-dir (v- closest (v vertices vertex)))
                   (orig-step (vlength move-dir))
                   (step orig-step)
                   (flag (< step 1e15))
                   (normal (nvunit (reduce #'nv+ (aref vertex-faces vertex)
                                           :initial-value (vec 0 0 0)
                                           :key (lambda (f) (aref face-normals f))))))
              (nv/ move-dir orig-step)
              (when flag
                (if (convex-p vertex normal)
                    (loop for face across (aref vertex-faces vertex)
                          do (when (< 0 (v. (aref face-normals face) move-dir))
                               (return (setf flag NIL))))
                    (loop for face across (aref vertex-faces vertex)
                          do (when (< (v. (aref face-normals face) move-dir) 0)
                               (return (setf flag T))))))
              (cond (flag
                     (setf step (min (* min-step len) step))
                     )
                    (T
                     (setf (aref invalid-indices vertex) (length invalid-vertices))
                     (vector-push-extend vertex invalid-vertices)
                     (setf (aref colors (+ 0 (* 3 vertex))) 0)
                     (setf (aref colors (+ 1 (* 3 vertex))) 0))))))))))

(defun normalize-vertices (vertices faces)
  (let ((displacements (make-array (length vertices) :element-type 'single-float))
        (weights (make-array (truncate (length vertices) 3) :element-type '(unsigned-byte 32))))
    (labels ((transfer (a b)
               (incf (aref displacements (+ 0 a)) (aref vertices (+ 0 b)))
               (incf (aref displacements (+ 1 a)) (aref vertices (+ 1 b)))
               (incf (aref displacements (+ 2 a)) (aref vertices (+ 2 b))))
             (record (a b)
               (incf (aref weights a))
               (incf (aref weights b))
               (transfer (* 3 a) (* 3 b))
               (transfer (* 3 b) (* 3 a))))
      (loop for i from 0 below (length faces) by 3
            for a = (aref faces (+ 0 i))
            for b = (aref faces (+ 1 i))
            for c = (aref faces (+ 2 i))
            do (record a b)
               (record b c)
               (record c a)))
    (loop for i from 0 below (length vertices) by 3
          for weight across weights
          for displacement across displacements
          do (when (< 0 weight)
               (let ((weight (/ weight)))
                 (setf (aref vertices (+ 0 i)) (* (vx displacement) weight))
                 (setf (aref vertices (+ 1 i)) (* (vy displacement) weight))
                 (setf (aref vertices (+ 2 i)) (* (vz displacement) weight))))))
  (values vertices faces))

(defun manifold (vertices faces &key resolution)
  (let ((tree (build-octtree vertices faces :resolution resolution)))
    (multiple-value-bind (mvertices mfaces) (multiple-value-call #'construct-triangle-mesh
                                              (construct-quad-manifold tree))
      (project-manifold mvertices mfaces vertices faces)
      (normalize-vertices mvertices mfaces))))
