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

(defun remove-degenerate-triangles (vertices indices &key (angle-threshold 0.01) (area-threshold 0.001)
                                                          (length-threshold 0.01))
  (declare (optimize speed (safety 1)))
  (check-type vertices vertex-array)
  (check-type indices face-array)
  (with-specialization (vertices vtype (vertex-array single-float) (vertex-array double-float))
    `(with-specialization (indices ftype (face-array (unsigned-byte 16)) (face-array (unsigned-byte 32)))
       `(let ((vertices (unsimplify vertices))
              (indices (unsimplify indices))
              (adjacency (unsimplify (face-adjacency-list indices)))
              (vfaces (unsimplify (vertex-faces indices)))
              (angle-threshold (coerce angle-threshold ',',(second vtype)))
              (area-threshold (coerce area-threshold ',',(second vtype))))
          (declare (type (unsimple-array ,(second ftype)) indices))
          (declare (type (unsimple-array ,',(second vtype)) vertices))
          (declare (type unsimple-array adjacency vfaces))
          (macrolet ((vfaces (face)
                       `(the unsimple-array (aref vfaces ,face))))
            (labels ((delete-triangle (face)
                       (declare (type face face))
                       (let* ((i (* 3 face))
                              (i0 (aref indices (+ i 0)))
                              (i1 (aref indices (+ i 1)))
                              (i2 (aref indices (+ i 2))))
                         (setf (aref vfaces i0) (delete face (vfaces i0)))
                         (setf (aref vfaces i1) (delete face (vfaces i1)))
                         (setf (aref vfaces i2) (delete face (vfaces i2)))
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
                       ;; Check if the winding order of an adjacent face
                       ;; is the same as the first two vertices (which form
                       ;; the edge of the adjacent. If so, we need to flip
                       ;; the winding order of this new triangle to match the
                       ;; face normal.
                       (when adjacent
                         (let* ((f (* 3 (first adjacent)))
                                (f0 (aref indices (+ f 0)))
                                (f1 (aref indices (+ f 1)))
                                (f2 (aref indices (+ f 2))))
                           (when (or (and (= f0 a) (= f1 b))
                                     (and (= f1 a) (= f2 b))
                                     (and (= f2 a) (= f0 b)))
                             (rotatef a b))))
                       ;; Create a new triangle while maintaining the maps
                       (let ((tri (truncate (length indices) 3)))
                         (vector-push-extend a indices)
                         (vector-push-extend b indices)
                         (vector-push-extend c indices)
                         (vector-push-extend adjacent adjacency)
                         (vector-push-extend tri (vfaces a))
                         (vector-push-extend tri (vfaces b))
                         (vector-push-extend tri (vfaces c))
                         tri))
                     (update-triangle (face old new)
                       (declare (type face face))
                       (let ((i (* 3 face)))
                         (flet ((try (i)
                                  (when (= old (aref indices i))
                                    (unless (find face (vfaces new))
                                      (vector-push-extend face (vfaces new)))
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
                         (loop for adjacent across (vfaces b)
                               do (update-triangle adjacent b a))
                         ;; Delete this triangle and all adjacents over the edge.
                         (dolist (adjacent (adjacent-faces face a b indices adjacency))
                           (delete-triangle adjacent))
                         (delete-triangle face)))
                     (split-edge (a b c face)
                       (declare (type face face))
                       ;; Split AB edge to C, create new triangles ACD, BCD
                       ;; where D is the opposing corner of any triangle over AB,
                       ;; mark the original triangles for deletion.
                       ;;
                       ;; This is messy because we update the adjacency map and vertex
                       ;; face map in-place to avoid recomputing them on each iteration
                       (vector-push-extend (make-array 0 :adjustable T :fill-pointer T) vfaces)
                       (let* ((adjacents (adjacent-faces face a b indices adjacency))
                              (cornering (vfaces c)))
                         (loop for face in adjacents
                               for d = (face-corner face a b indices)
                               for al = (make-triangle d a c (adjacent-faces face d a indices adjacency))
                               for ar = (make-triangle d b c (adjacent-faces face d b indices adjacency))
                               do (push al (aref adjacency ar))
                                  (push ar (aref adjacency al))
                                  (loop for face across cornering
                                        do (cond ((face-edge-p indices face a c)
                                                  (push al (aref adjacency face)))
                                                 ((face-edge-p indices face b c)
                                                  (push ar (aref adjacency face))))))
                         (delete-triangle face)
                         (mapc #'delete-triangle adjacents)))
                     (consider-area (a b c face)
                       (let ((ap (v vertices a))
                             (bp (v vertices b))
                             (cp (v vertices c)))
                         (when (< 0 (triangle-area ap bp cp) area-threshold)
                           (let ((a-d (vdistance ap bp))
                                 (b-d (vdistance bp cp))
                                 (c-d (vdistance cp ap)))
                             ;; Find smallest edge and fuse it
                             (cond ((and (< a-d b-d) (< a-d c-d) (< a-d length-threshold))
                                    (fuse-edge a b face))
                                   ((and (< b-d a-d) (< b-d c-d) (< b-d length-threshold))
                                    (fuse-edge b c face))
                                   ((< c-d length-threshold)
                                    (fuse-edge c a face))
                                   ;; No edge was small enough to be fused away safely.
                                   ;; We instead will try to dissolve the longest edge.
                                   ((and (< b-d a-d) (< c-d a-d))
                                    (split-edge a b c face))
                                   ((and (< a-d b-d) (< c-d b-d))
                                    (split-edge b c a face))
                                   (T
                                    (split-edge c a b face)))))))
                     (consider-angle (corner a b face)
                       ;; Consider one corner of the triangle for merging
                       (let* ((cp (v vertices corner))
                              (ap (v vertices a))
                              (bp (v vertices b))
                              (c-a (v- ap cp))
                              (c-b (v- bp cp)))
                         ;; Make sure we don't consider zero-area triangles at all
                         (when (and (v/= c-a 0) (v/= c-b 0) (< (vangle c-a c-b) angle-threshold))
                           (let ((a-d (vdistance cp ap))
                                 (b-d (vdistance cp bp))
                                 (ab-d (vdistance ap bp)))
                             ;; If the opposing edge is the smallest, and it isn't large
                             ;; by itself either, fuse it, otherwise split the longer edge
                             ;; as it is more likely to not lead to further degenerate triangles.
                             (cond ((and (< ab-d a-d) (< ab-d b-d)
                                         (< ab-d length-threshold))
                                    (fuse-edge a b face))
                                   ((< a-d b-d)
                                    (split-edge corner b a face))
                                   (T
                                    (split-edge corner a b face))))
                           T))))
              ;; In the first step, we loop and generate new vertices and new triangles
              ;; until there aren't any changes made anymore.
              (tagbody retry
                 (loop with found = NIL
                       for i of-type face from 0 below (length indices) by 3
                       for face of-type face from 0
                       for p1 = (aref indices (+ i 0))
                       for p2 = (aref indices (+ i 1))
                       for p3 = (aref indices (+ i 2))
                       do (when (and (/= p1 p2) (/= p1 p3) (/= p2 p3)
                                     (or (consider-area p1 p2 p3 face)
                                         (consider-angle p1 p2 p3 face)
                                         (consider-angle p2 p1 p3 face)
                                         (consider-angle p3 p1 p2 face)))
                            (setf found T))
                       finally (when found (go retry))))
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

(defun normalize (vertices indices &key (threshold 0.001) (center (vec 0 0 0)) (scale 1.0) (angle-threshold 0.001) (area-threshold 0.001))
  (when (and threshold (< 0 threshold))
    (multiple-value-setq (vertices indices) (remove-duplicate-vertices vertices indices
                                                                       :threshold threshold
                                                                       :center center
                                                                       :scale scale)))
  (if (or (and angle-threshold (< 0 angle-threshold))
          (and area-threshold (< 0 area-threshold)))
      (multiple-value-setq (vertices indices) (remove-degenerate-triangles vertices indices
                                                                           :angle-threshold (or angle-threshold 0.0)
                                                                           :area-threshold (* scale (or area-threshold 0.0))))
      (multiple-value-setq (vertices indices) (remove-unused vertices indices)))
  (values vertices indices))
