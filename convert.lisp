#|
 This file is a part of manifolds
 (c) 2023 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.manifolds)

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
        (vertices (make-array 0 :adjustable T :fill-pointer T))
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
                     (setf id (length vertices))
                     (setf (gethash vid vcolor) id)
                     (vector-push-extend d vertices)
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

(defun construct-triangle-mesh (vcolor vertices quad-faces vertex-faces)
  )

(defun find-closest-point (vertices faces vertex-faces vertex)
  (let ((cpoint (vec 1e20 1e20 1e20))
        (normal (vec 0 0 0))
        (v (aref vertices vertex)))
    (loop for face across (aref vertex-faces vertex)
          do (let ((p (closest-point-on-triangle vertices faces face v)))
               (when (< (vdistance p v) (vdistance cpoint v))
                 (setf normal (face-normal* vertices faces face))
                 (when (< (v. normal (v- v cpoint)) 0)
                   (nv- normal))
                 (v<- cpoint p))))
    (nv+* cpoint normal 5e-4)))

(defun project-manifold (vertices faces orig-vertices orig-faces orig-vertex-faces &key (iterations 20))
  (let* ((vertex-count (length vertices))
         (len (min (vdistance (aref vertices (aref faces 0)) (aref vertices (aref faces 1)))
                   (vdistance (aref vertices (aref faces 0)) (aref vertices (aref faces 2)))))
         (vertex-faces (vertex-faces faces (make-array vertex-count)))
         (face-normals (make-array (truncate (length faces) 3)))
         (invalid-vertices (make-array 0 :element-type '(unsigned-byte 32) :adjustable T :fill-pointer T))
         (invalid-indices (make-array (length vertex-faces) :element-type '(signed-byte 32) :initial-element -1))
         (visited (make-array (length vertex-faces) :element-type 'bit))
         (min-step (/ 2.0 iterations)))
    (labels ((convex-p (vertex normal)
               (loop for face across (aref vertex-faces vertex)
                     do (dotimes (i 3)
                          (when (< 0 (v. (v- (aref vertices (aref faces (+ i (* 3 face))))
                                             (aref vertices vertex))
                                         normal))
                            (return-from convex-p NIL)))
                     finally (return T)))
             (mark-invalid (vertex)
               (setf (aref invalid-indices vertex) (length invalid-vertices))
               (vector-push-extend vertex invalid-vertices)))
      ;; Main projection loop
      (dotimes (iter iterations)
        (face-normals* vertices faces face-normals)
        (dotimes (vertex vertex-count)
          (when (= 0 (sbit visited vertex))
            (let* ((v-faces (aref vertex-faces vertex))
                   (closest (find-closest-point orig-vertices orig-faces orig-vertex-faces vertex))
                   (move-dir (v- closest (aref vertices vertex)))
                   (orig-step (vlength move-dir))
                   (step orig-step)
                   (flag (< step 1e15))
                   (normal (nvunit (reduce #'nv+ v-faces
                                           :initial-value (vec 0 0 0)
                                           :key (lambda (f) (aref face-normals f))))))
              (nv/ move-dir orig-step)
              (when flag
                (if (convex-p vertex normal)
                    (loop for face across v-faces
                          do (when (< 0 (v. (aref face-normals face) move-dir))
                               (return (setf flag NIL))))
                    (loop for face across v-faces
                          do (when (< (v. (aref face-normals face) move-dir) 0)
                               (return (setf flag T))))))
              (cond (flag
                     (setf step (min (* min-step len) step))
                     ;; Almost exactly the same as below...
                     (loop for face across v-faces
                           for a = (aref faces (+ 0 (* 3 face)))
                           for b = (aref faces (+ 1 (* 3 face)))
                           for c = (aref faces (+ 2 (* 3 face)))
                           do (loop until (= a vertex)
                                    do (rotatef a b c))
                              (let* ((dir (nvunit (v- (aref vertices c) (aref vertices b))))
                                     (h (v+ (aref vertices b)
                                            (v* (v. (v- (aref vertices a) (aref vertices b)) dir) dir)
                                            (v- (aref vertices a))))
                                     (h-len (vlength h)))
                                (nv/ h h-len)
                                (let ((h-step (* (v. h move-dir) step)))
                                  (when (< (* 0.7 h-len) h-step)
                                    (setf step (* step h-len 0.7 (/ h-step)))
                                    (mark-invalid vertex)))))
                     (cond ((< (abs (- step orig-step)) 1e-6)
                            (v<- (aref vertices vertex) closest)
                            (nv+* (aref vertices vertex) normal len)
                            (setf step (max 0.0 (- step 1e-4)))
                            (setf (sbit visited vertex) 1))
                           (T
                            (nv+* (aref vertices vertex) move-dir step)))
                     (loop for face across v-faces
                           do (setf (aref face-normals face) (face-normal* vertices faces face))))
                    (T
                     (mark-invalid vertex)))))))
      ;; Fixup invalid vertices
      (fill visited 0)
      (dotimes (i (length invalid-vertices))
        (when (= 0 (sbit visited i))
          (setf (sbit visited i) 1)
          (let ((queue (make-array 0 :adjustable T :fill-pointer T)))
            (vector-push-extend i queue)
            (loop for f from 0
                  while (< f (length queue))
                  do (dolist (face (aref vertex-faces (aref queue f)))
                       (loop for fi from (* 3 face) repeat 3
                             for index = (aref invalid-indices (aref faces fi))
                             do (when (and (/= -1 index) (not (sbitp visited index)))
                                  (setf (sbitp visited index) T)
                                  (vector-push-extend index queue)))))
            (loop for it across queue
                  for midpoint = (vec 0 0 0)
                  for count = 0
                  for vertex = (aref invalid-vertices it)
                  for v-faces = (aref vertex-faces vertex)
                  do (dolist (face v-faces)
                       (loop for fi from (* 3 face) repeat 3
                             for index = (aref faces fi)
                             do (when (or (= -1 (aref invalid-indices index))
                                          (not (sbitp visited (aref invalid-indices index))))
                                  (nv+ midpoint (aref vertices index))
                                  (incf count))))
                     (setf (sbitp visited it) NIL)
                     (let* ((move-dir (v- (v/ midpoint count) (aref vertices vertex)))
                            (step (vlength move-dir)))
                       (unless (or (= 0 step) (= 0 count))
                         (nv/ move-dir step)
                         ;; Almost exactly the same as above...
                         (loop for face across v-faces
                               for a = (aref faces (+ 0 (* 3 face)))
                               for b = (aref faces (+ 1 (* 3 face)))
                               for c = (aref faces (+ 2 (* 3 face)))
                               do (loop until (= a vertex)
                                        do (rotatef a b c))
                                  (let* ((dir (nvunit (v- (aref vertices c) (aref vertices b))))
                                         (h (v+ (aref vertices b)
                                                (v* (v. (v- (aref vertices a) (aref vertices b)) dir) dir)
                                                (v- (aref vertices a))))
                                         (h-len (vlength h)))
                                    (nv/ h h-len)
                                    (let ((h-step (* (v. h move-dir) step)))
                                      (when (< (* 0.7 h-len) h-step)
                                        (setf step (* step h-len 0.7 (/ h-step)))))))
                         (nv+* (aref vertices vertex) move-dir step))))
            (loop for index across queue
                  do (setf (sbitp visited index) T))))))))

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
