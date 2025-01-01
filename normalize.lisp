(in-package #:org.shirakumo.fraf.manifolds)

(defstruct (vertex-index
            (:include vec3)
            (:constructor %make-vertex-index (varr3 index))
            (:copier NIL)
            (:predicate NIL))
  (index 0 :type (unsigned-byte 32) :read-only T))

(defmethod org.shirakumo.fraf.trial.space:location ((object vertex-index))
  object)

(defmethod org.shirakumo.fraf.trial.space:bsize ((object vertex-index))
  (vec 0 0 0))

(defun remove-unused (vertices indices)
  (declare (optimize speed (safety 1)))
  (check-type vertices vertex-array)
  (check-type indices face-array)
  (let ((used (make-array (truncate (length vertices) 3) :initial-element 0 :element-type 'bit))
        (face-count 0))
    (declare (type (unsigned-byte 32) face-count))
    (with-face-specialization (indices)
      (with-vertex-specialization (vertices)
        (flet ((real-face-p (i0 i1 i2)
                 (and (/= i0 i1) (/= i1 i2) (/= i0 i2)
                      (< i0 (length used)) (< i1 (length used)) (< i2 (length used)))))
          ;; First, mark used vertices in a bitmap
          (loop for i from 0 below (length indices) by 3
                for i0 = (aref indices (+ i 0))
                for i1 = (aref indices (+ i 1))
                for i2 = (aref indices (+ i 2))
                do (when (real-face-p i0 i1 i2)
                     (setf (sbitp used i0) T)
                     (setf (sbitp used i1) T)
                     (setf (sbitp used i2) T)
                     (incf face-count)))
          (let ((new-vertices (make-array 0 :element-type (array-element-type vertices) :adjustable T :fill-pointer T))
                (index-map (make-array (length used) :element-type (array-element-type indices)))
                (new-indices (make-array (* 3 face-count) :element-type (array-element-type indices))))
            ;; Now compact the vertex array and keep track of index rewrites
            (loop with new-index = 0
                  for old-index from 0 below (length used)
                  do (when (sbitp used old-index)
                       (vector-push-extend (aref vertices (+ (* 3 old-index) 0)) new-vertices)
                       (vector-push-extend (aref vertices (+ (* 3 old-index) 1)) new-vertices)
                       (vector-push-extend (aref vertices (+ (* 3 old-index) 2)) new-vertices)
                       (setf (aref index-map old-index) new-index)
                       (incf new-index)))
            ;; Rewrite the vertex indices
            (loop with fi = 0
                  for i from 0 below (length indices) by 3
                  for i0 = (aref indices (+ i 0))
                  for i1 = (aref indices (+ i 1))
                  for i2 = (aref indices (+ i 2))
                  do (when (real-face-p i0 i1 i2)
                       (setf (aref new-indices (+ fi 0)) (aref index-map i0))
                       (setf (aref new-indices (+ fi 1)) (aref index-map i1))
                       (setf (aref new-indices (+ fi 2)) (aref index-map i2))
                       (incf fi 3)))
            (values (simplify new-vertices) new-indices)))))))

(defun remove-degenerate-triangles (vertices indices &key (threshold 0.01))
  (declare (optimize speed (safety 1)))
  (check-type vertices vertex-array)
  (check-type indices face-array)
  (with-specialization (vertices vtype (vertex-array single-float) (vertex-array double-float))
    `(with-specialization (indices ftype (face-array (unsigned-byte 16)) (face-array (unsigned-byte 32)))
       `(let ((vertices (unsimplify vertices))
              (indices (unsimplify indices))
              (adjacency (unsimplify (face-adjacency-list indices)))
              (vfaces (unsimplify (vertex-faces indices)))
              (threshold (coerce threshold ',',(second vtype))))
          (declare (type (unsimple-array ,(second ftype)) indices))
          (declare (type (unsimple-array ,',(second vtype)) vertices))
          (declare (type unsimple-array adjacency vfaces))
          (macrolet ((vface (face)
                       `(the unsimple-array (aref vfaces ,face))))
            (labels ((delete-triangle (face)
                       (declare (type face face))
                       (let* ((i (* 3 face))
                              (i0 (aref indices (+ i 0)))
                              (i1 (aref indices (+ i 1)))
                              (i2 (aref indices (+ i 2))))
                         (setf (aref vfaces i0) (delete face (vface i0)))
                         (setf (aref vfaces i1) (delete face (vface i1)))
                         (setf (aref vfaces i2) (delete face (vface i2)))
                         ;; Setting the triangle vertices to the same will cause it
                         ;; to be deleted later in DELETE-UNUSED.
                         (setf (aref indices (+ i 0)) 0)
                         (setf (aref indices (+ i 1)) 0)
                         (setf (aref indices (+ i 2)) 0))
                       ;; Delete from adjacency maps
                       (dolist (adjacent (aref adjacency face))
                         (setf (aref adjacency adjacent) (delete face (the list (aref adjacency adjacent)))))
                       (setf (aref adjacency face) ()))
                     (make-triangle (a b c adjacent)
                       ;; Create a new triangle while maintaining the maps
                       (let ((tri (truncate (length indices) 3)))
                         (vector-push-extend a indices)
                         (vector-push-extend b indices)
                         (vector-push-extend c indices)
                         (vector-push-extend adjacent adjacency)
                         (vector-push-extend tri (vface a))
                         (vector-push-extend tri (vface b))
                         (vector-push-extend tri (vface c))
                         tri))
                     (update-triangle (face old new)
                       (declare (type face face))
                       (let ((i (* 3 face)))
                         (flet ((try (i)
                                  (when (= old (aref indices i))
                                    (unless (find face (vface new))
                                      (vector-push-extend face (vface new)))
                                    (setf (aref indices i) new))))
                           (try (+ i 0))
                           (try (+ i 1))
                           (try (+ i 2)))))
                     (fuse-edge (a b face)
                       (declare (type face face))
                       (let ((mid (nv* (nv+ (v vertices a) (v vertices b)) 0.5)))
                         ;; Set the vertex to the midpoint, zero out the old one.
                         (setf (v vertices a) mid)
                         (setf (v vertices b) (vec 0 0 0))
                         ;; Update all involved triangles to point to A
                         (loop for adjacent across (vface b)
                               do (update-triangle adjacent b a))
                         ;; Delete this triangle and all adjacents over the edge.
                         (dolist (adjacent (adjacent-faces face a b indices adjacency))
                           (delete-triangle adjacent))
                         (delete-triangle face)))
                     (split-edge (a b c face)
                       (declare (type face face))
                       ;; Split AB edge to M, create new triangles AMC, BMC, AMD, BMD
                       ;; where D is the opposing corner of any triangle over AB, and
                       ;; mark the original triangles for deletion.
                       ;;
                       ;; This is messy because we update the adjacency map and vertex
                       ;; face map in-place to avoid recomputing them on each iteration
                       (let ((mid (nv* (nv+ (v vertices a) (v vertices b)) 0.5))
                             (m (truncate (length vertices) 3)))
                         (vector-push-extend (vx mid) vertices)
                         (vector-push-extend (vy mid) vertices)
                         (vector-push-extend (vz mid) vertices)
                         (vector-push-extend (make-array 0 :adjustable T :fill-pointer T) vfaces)
                         (let* ((adjacents (adjacent-faces face a b indices adjacency))
                                (l (make-triangle c m a (adjacent-faces face c a indices adjacency)))
                                (r (make-triangle c b m (adjacent-faces face c b indices adjacency))))
                           (push l (aref adjacency r))
                           (push r (aref adjacency l))
                           (loop for face in adjacents
                                 for d = (face-corner face a b indices)
                                 for al = (make-triangle d m a (adjacent-faces face d a indices adjacency))
                                 for ar = (make-triangle d b m (adjacent-faces face d b indices adjacency))
                                 do (push al (aref adjacency ar))
                                    (push ar (aref adjacency al))
                                    (push l (aref adjacency al))
                                    (push r (aref adjacency ar))
                                    (push al (aref adjacency l))
                                    (push ar (aref adjacency r)))
                           (delete-triangle face)
                           (mapc #'delete-triangle adjacents))))
                     (consider (corner a b face)
                       ;; Consider one corner of the triangle for merging
                       (let* ((cp (v vertices corner))
                              (ap (v vertices a))
                              (bp (v vertices b))
                              (c-a (v- ap cp))
                              (c-b (v- bp cp)))
                         ;; Make sure we don't consider zero-area triangles at all
                         (when (and (v/= c-a 0) (v/= c-b 0) (< (vangle c-a c-b) threshold))
                           (let ((a-d (vdistance cp ap))
                                 (b-d (vdistance cp bp))
                                 (ab-d (vdistance ap bp)))
                             ;; If the opposing edge is the smallest, fuse it, otherwise
                             ;; split the longer edge as it is more likely to not lead to
                             ;; further degenerate triangles.
                             (cond ((and (< ab-d a-d) (< ab-d b-d))
                                    (fuse-edge a b face))
                                   ((< a-d b-d)
                                    (split-edge corner b a face))
                                   (T
                                    (split-edge corner a b face))))
                           T))))
              ;; In the first step, we loop and generate new vertices and new triangles
              ;; until there aren't any changes made anymore.
              (tagbody retry
                 (loop for i of-type face from 0 below (length indices) by 3
                       for face of-type face from 0
                       for p1 = (aref indices (+ i 0))
                       for p2 = (aref indices (+ i 1))
                       for p3 = (aref indices (+ i 2))
                       do (when (and (/= p1 p2) (/= p1 p3) (/= p2 p3)
                                     (or (consider p1 p2 p3 face)
                                         (consider p2 p1 p3 face)
                                         (consider p3 p1 p2 face)))
                            ;; We have a change made. We have to retry **now**
                            ;; as continuing would potentially confuse the algorithm
                            ;; with outdated information.
                            (go retry))))
              (remove-unused (simplify vertices) (simplify indices))))))))

(defun remove-duplicate-vertices (vertices indices &key (threshold 0.001) (center (vec3 0)) (scale 1.0))
  (check-type vertices vertex-array)
  (check-type indices face-array)
  ;; TODO: could probably do this inline by going over verts first, then faces and only copying once.
  (with-face-specialization (indices)
    (with-vertex-specialization (vertices)
      (let ((center (case vertex-component-type
                      (double-float (dvec center))
                      (single-float center)))
            (tree (org.shirakumo.fraf.trial.space.kd-tree:make-kd-tree))
            (new-vertices (make-array 0 :element-type vertex-component-type :adjustable T :fill-pointer T))
            (new-indices (make-array 0 :element-type 'u32 :adjustable T :fill-pointer T)))
        (flet ((vertex-idx (p)
                 (let* ((p (vec (nv* (nv- p center) scale)))
                        (nearest (org.shirakumo.fraf.trial.space.kd-tree:kd-tree-nearest p tree :max-radius threshold)))
                   (cond (nearest
                          (vertex-index-index nearest))
                         (T
                          (let ((index (truncate (length new-vertices) 3)))
                            (org.shirakumo.fraf.trial.space.kd-tree:kd-tree-insert
                             (%make-vertex-index (varr3 p) index) tree)
                            (vector-push-extend (coerce (vx p) vertex-component-type) new-vertices)
                            (vector-push-extend (coerce (vy p) vertex-component-type) new-vertices)
                            (vector-push-extend (coerce (vz p) vertex-component-type) new-vertices)
                            index))))))
          (loop for i from 0 below (length indices) by 3
                for p1 = (v vertices (aref indices (+ i 0)))
                for p2 = (v vertices (aref indices (+ i 1)))
                for p3 = (v vertices (aref indices (+ i 2)))
                for i1 = (vertex-idx p1)
                for i2 = (vertex-idx p2)
                for i3 = (vertex-idx p3)
                do (when (and (/= i1 i2) (/= i1 i3) (/= i2 i3))
                     (vector-push-extend i1 new-indices)
                     (vector-push-extend i2 new-indices)
                     (vector-push-extend i3 new-indices))))
        (values (simplify new-vertices) (simplify new-indices))))))

(defun normalize (vertices indices &key (threshold 0.001) (center (vec 0 0 0)) (scale 1.0) (angle-threshold 0.001))
  (multiple-value-setq (vertices indices) (remove-duplicate-vertices vertices indices :threshold threshold :center center :scale scale))
  (if (and angle-threshold (< 0 angle-threshold))
      (multiple-value-setq (vertices indices) (remove-degenerate-triangles vertices indices :threshold angle-threshold))
      (multiple-value-setq (vertices indices) (remove-unused vertices indices)))
  (values vertices indices))
