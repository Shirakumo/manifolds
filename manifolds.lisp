(in-package #:org.shirakumo.fraf.manifolds)

(declaim (inline u32 ensure-u32 f32* f32 ensure-f32 f64* f64 ensure-f64))

(defun u32 (&rest i)
  (make-array (length i) :element-type '(unsigned-byte 32) :initial-contents i))

(defun ensure-u32 (a)
  (etypecase a
    ((simple-array (unsigned-byte 32) (*))
     a)
    (vector
     (make-array (length a) :element-type '(unsigned-byte 32) :initial-contents a))))

(defun f32* (a)
  (float a 0f0))

(defun f32 (&rest i)
  (make-array (length i) :element-type 'single-float :initial-contents (mapcar #'float i)))

(defun ensure-f32 (a)
  (etypecase a
    ((simple-array single-float (*))
     a)
    (vector
     (make-array (length a) :element-type 'single-float :initial-contents a))))

(defun f64* (a)
  (float a 0d0))

(defun f64 (&rest i)
  (make-array (length i) :element-type 'double-float :initial-contents (mapcar #'float i)))

(defun ensure-f64 (a)
  (etypecase a
    ((simple-array double-float (*))
     a)
    (vector
     (make-array (length a) :element-type 'double-float :initial-contents a))))

(defmacro do-faces ((a b c faces &optional result) &body body)
  (let ((i (gensym "I"))
        (f (gensym "F")))
    `(loop with ,f = ,faces
           for ,i from 0 below (length ,f) by 3
           for ,a = (aref ,f (+ 0 ,i))
           for ,b = (aref ,f (+ 1 ,i))
           for ,c = (aref ,f (+ 2 ,i))
           do (progn ,@body)
           finally (return ,result))))

(defmacro do-directions ((x y z &optional (min 0) (max 1) result) &body body)
  (let ((thunk (gensym "THUNK")))
    `(block NIL
       (flet ((,thunk (,x ,y ,z)
                ,@body))
         (,thunk ,min ,min ,min)
         (,thunk ,min ,min ,max)
         (,thunk ,min ,max ,min)
         (,thunk ,min ,max ,max)
         (,thunk ,max ,min ,min)
         (,thunk ,max ,min ,max)
         (,thunk ,max ,max ,min)
         (,thunk ,max ,max ,max)
         ,result))))

(defun vertex-adjacency-list (faces &optional adjacency)
  (check-type faces (simple-array (unsigned-byte 32) (*)))
  (let ((adjacency (or adjacency
                       (make-array (1+ (loop for idx across faces maximize idx)) :initial-element ()))))
    (do-faces (a b c faces adjacency)
      (pushnew b (aref adjacency a))
      (pushnew c (aref adjacency a))
      (pushnew a (aref adjacency b))
      (pushnew c (aref adjacency b))
      (pushnew a (aref adjacency c))
      (pushnew b (aref adjacency c)))))

(defun half-edge-list (faces)
  (check-type faces (simple-array (unsigned-byte 32) (*)))
  (let ((edges (make-array (length faces) :element-type 'cons))
        (i 0))
    (flet ((edge (a b)
             (setf (aref edges i) (cons a b))
             (incf i)))
      (do-faces (a b c faces edges)
        (edge a b)
        (edge b c)
        (edge c a)))))

(defun edge-list (faces)
  (check-type faces (simple-array (unsigned-byte 32) (*)))
  (let ((edge-table (make-hash-table :test 'eql))
        (edges (make-array 0 :element-type 'cons :adjustable T :fill-pointer T)))
    (flet ((edge (a b)
             (if (< a b) 
                 (pushnew b (gethash a edge-table))
                 (pushnew a (gethash b edge-table)))))
      (do-faces (a b c faces)
        (edge a b)
        (edge b c)
        (edge c a))
      (loop for a being the hash-keys of edge-table using (hash-value bs)
            do (dolist (b bs)
                 (vector-push-extend (cons a b) edges)))
      edges)))

(declaim (inline v (setf v) sbitp (setf sbitp)))
(defun v (vertices i)
  (let ((i (* 3 i)))
    (vec (aref vertices (+ 0 i))
         (aref vertices (+ 1 i))
         (aref vertices (+ 2 i)))))

(defun (setf v) (v vertices i)
  (let ((i (* 3 i)))
    (ecase (array-element-type vertices)
      (single-float
       (setf (aref vertices (+ 0 i)) (vx3 v))
       (setf (aref vertices (+ 1 i)) (vy3 v))
       (setf (aref vertices (+ 2 i)) (vz3 v)))
      (double-float
       (setf (aref vertices (+ 0 i)) (float (vx3 v) 0d0))
       (setf (aref vertices (+ 1 i)) (float (vy3 v) 0d0))
       (setf (aref vertices (+ 2 i)) (float (vz3 v) 0d0))))
    v))

(defun sbitp (array i)
  (= 1 (sbit array i)))

(defun (setf sbitp) (value array i)
  (setf (sbit array i) (if value 1 0)))

(defun face-normal (vertices faces face)
  (let* ((i (* 3 face))
         (a (v vertices (aref faces (+ 0 i))))
         (b (v vertices (aref faces (+ 1 i))))
         (c (v vertices (aref faces (+ 2 i)))))
    (vc (v- b a) (v- c a))))

(defun face-normal* (vertices faces face)
  (let* ((i (* 3 face))
         (a (aref vertices (aref faces (+ 0 i))))
         (b (aref vertices (aref faces (+ 1 i))))
         (c (aref vertices (aref faces (+ 2 i)))))
    (vc (v- b a) (v- c a))))

(defun face-normals (vertices faces &optional (face-normals (make-array (truncate (length faces) 3))))
  (dotimes (i (length face-normals) face-normals)
    (setf (aref face-normals i) (face-normal vertices faces i))))

(defun face-normals* (vertices faces &optional (face-normals (make-array (truncate (length faces) 3))))
  (dotimes (i (length face-normals) face-normals)
    (setf (aref face-normals i) (face-normal* vertices faces i))))

(defun face-area (vertices faces face)
  (let* ((i (* 3 face))
         (p1 (v vertices (aref faces (+ 0 i))))
         (p2 (v vertices (aref faces (+ 1 i))))
         (p3 (v vertices (aref faces (+ 2 i)))))
    (let* ((base (v2norm (v- p2 p1)))
           (height (if (= 0.0 base) 0.0
                       (v2norm (v- p3 p1 (v* (v- p2 p1) (/ (v. (v- p3 p1) (v- p2 p1))
                                                           (* base base))))))))
      (* 0.5 base height))))

(defun centroid (vertices faces)
  (let ((numerator (vec 0 0 0))
        (denominator 0.0))
    (loop for i from 0 below (length faces) by 3
          for i1 = (aref faces (+ 0 i))
          for i2 = (aref faces (+ 1 i))
          for i3 = (aref faces (+ 2 i))
          for sum = (v/ (v+ (v vertices i1) (v vertices i2) (v vertices i3)) 3)
          for area = (face-area vertices faces (truncate i 3))
          do (nv+ numerator (v* sum area))
             (incf denominator area))
    (if (= denominator 0.0)
        numerator
        (nv* numerator (/ denominator)))))

(defun convex-volume (vertices faces)
  (let ((bary (vec3)))
    (loop for i from 0 below (length vertices) by 3
          do (incf (vx bary) (aref vertices (+ i 0)))
             (incf (vy bary) (aref vertices (+ i 1)))
             (incf (vz bary) (aref vertices (+ i 2))))
    (nv/ bary (truncate (length vertices) 3))
    (flet ((volume4 (a b c d)
             (v. (v- a d) (vc (v- b d) (v- c d)))))
      (/ (loop for i from 0 below (length faces) by 3
               sum (volume4 (v vertices (aref faces (+ i 0)))
                            (v vertices (aref faces (+ i 1)))
                            (v vertices (aref faces (+ i 2)))
                            bary))
         6.0))))

(defun closest-point-on-triangle (vertices faces face point)
  (let* ((v0 (v vertices (aref faces (+ 0 (* face 3)))))
         (e0 (nv- (v vertices (aref faces (+ 1 (* face 3)))) v0))
         (e1 (nv- (v vertices (aref faces (+ 2 (* face 3)))) v0))
         (v0 (nv- v0 point))
         (a (v. e0 e0))
         (b (v. e0 e1))
         (c (v. e1 e1))
         (d (v. e0 v0))
         (e (v. e1 v0))
         (det (- (* a c) (* b b)))
         (idet (/ det))
         (u (- (* b e) (* c d)))
         (v (- (* b d) (* a e))))
    ;; FIXME: There's lots of common subexpressions here...
    ;;        should be able to reduce this mess, no?
    (if (< (+ u v) det)
        (cond ((< u 0)
               (if (< v 0)
                   (if (< d 0)
                       (setf u (/ (- d) a)
                             v 0.0)
                       (setf u 0.0
                             v (/ (- e) c)))
                   (setf u 0.0
                         v (/ (- e) c))))
              ((< v 0)
               (setf u (/ (- d) a)
                     v 0.0))
              (T
               (setf u (* u idet))
               (setf v (* u v))))
        (cond ((< u 0)
               (if (< (+ b d) (+ c e))
                   (setf u (/ (- (+ c e) b d) (+ a (* -2 b) c))
                         v (- 1 u))
                   (setf u 0.0
                         v (/ (- e) c))))
              ((< v 0)
               (if (< (+ b e) (+ a d))
                   (setf u (/ (- (+ c e) b d) (+ a (* -2 b) c))
                         v (- 1 u))
                   (setf u (/ (- e) c)
                         v 0.0)))
              (T
               (setf u (/ (- (+ c e) b d) (+ a (* -2 b) c))
                     v (- 1 u)))))
    (flet ((clamp (x)
             (max 0.0 (min 1.0 x))))
      (nv+* (nv+* (v vertices (aref faces (* face 3))) e0 (clamp u)) e1 (clamp v)))))

(defun face-in-volume-p (vertices faces face c e)
  (declare (type (unsigned-byte 32) face))
  (declare (type vec3 c e))
  ;; SAT
  (let* ((v0 (v vertices (aref faces (+ 0 (* face 3)))))
         (v1 (v vertices (aref faces (+ 1 (* face 3)))))
         (v2 (v vertices (aref faces (+ 2 (* face 3)))))
         (v0 (v- v0 c)) (v1 (v- v1 c)) (v2 (v- v2 c))
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
    (loop for face from 0 below (truncate (length faces) 3)
          for j from 0 by 3
          do (when (face-in-volume-p vertices faces face location bsize)
               (vector-push-extend (aref faces (+ 0 j)) new-faces)
               (vector-push-extend (aref faces (+ 1 j)) new-faces)
               (vector-push-extend (aref faces (+ 2 j)) new-faces)))
    (make-array (length new-faces) :element-type '(unsigned-byte 32) :initial-contents new-faces)))

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

(defun vertex-faces (faces &optional vertex-faces)
  (let ((vertex-faces (or vertex-faces
                          (make-array (1+ (loop for vertex across faces maximize vertex))))))
    (dotimes (i (length vertex-faces))
      (setf (aref vertex-faces i) (make-array 0 :adjustable T :fill-pointer T)))
    (dotimes (i (length faces) vertex-faces)
      (vector-push-extend (truncate i 3) (aref vertex-faces (aref faces i))))))

(defun 2-manifold-p (faces &optional adjacency)
  (check-type faces (simple-array (unsigned-byte 32) (*)))
  (let ((edge-table (make-hash-table :test 'eql)))
    (flet ((edge-twofaced-p (a b)
             (let ((edge (if (< a b)
                             (+ a (ash b 32))
                             (+ b (ash a 32)))))
               (<= (incf (gethash edge edge-table 0)) 2)))
           (edge-loop-p (adjacency incident)
             (let* ((vertices (aref adjacency incident))
                    (visited (make-hash-table :test 'eql))
                    (stack (list (first vertices))))
               (loop for next = (pop stack)
                     while next
                     do (setf (gethash next visited) T)
                        (dolist (adjacent (aref adjacency next))
                          (when (and (not (gethash adjacent visited))
                                     (member adjacent vertices))
                            (push adjacent stack))))
               (loop for vertex in vertices
                     always (gethash vertex visited)))))
      (and (do-faces (a b c faces T)
             (unless (and (edge-twofaced-p a b)
                          (edge-twofaced-p b c)
                          (edge-twofaced-p c a))
               (return NIL)))
           (loop initially (unless adjacency (setf adjacency (vertex-adjacency-list faces)))
                 for vertex from 0 below (length adjacency)
                 always (edge-loop-p adjacency vertex))))))

(defun separate-meshes (vertices faces)
  (let ((mesh-count (make-array 0 :element-type '(unsigned-byte 32) :fill-pointer T :adjustable T))
        (vertex-mesh (make-array (truncate (length vertices) 3) :element-type '(unsigned-byte 32)
                                                                :initial-element 0))
        (adjacents (vertex-adjacency-list faces))
        (queue (make-array 0 :element-type '(unsigned-byte 32) :fill-pointer T :adjustable T)))
    (dotimes (i (length vertex-mesh))
      (when (= 0 (aref vertex-mesh i))
        (vector-push-extend 0 mesh-count)
        (vector-push-extend i queue)
        (loop with mesh-index = (length mesh-count)
              while (< 0 (length queue))
              for vertex = (vector-pop queue)
              do (cond ((= 0 (aref vertex-mesh vertex))
                        (setf (aref vertex-mesh vertex) mesh-index)
                        (incf (aref mesh-count (1- mesh-index)))
                        (dolist (adjacent adjacents)
                          (vector-push-extend adjacent queue)))
                       ((/= mesh-index (aref vertex-mesh vertex))
                        (error "Mesh is not manifold, as vertex ~a shares two meshes." vertex))))))
    (loop for mesh-index from 0
          for size across mesh-count
          for mesh-vertices = (make-array size :element-type 'single-float)
          for mesh-faces = (make-array 0 :element-type '(unsigned-byte 32) :adjustable T :fill-pointer T)
          do (loop for i from 0 below (length vertices) by 3
                   for j from 0
                   do (when (= mesh-index (aref vertex-mesh j))
                        (setf (aref mesh-vertices (+ i 0)) (aref vertices (+ i 0)))
                        (setf (aref mesh-vertices (+ i 1)) (aref vertices (+ i 1)))
                        (setf (aref mesh-vertices (+ i 2)) (aref vertices (+ i 2)))))
             (do-faces (a b c faces)
               (when (= mesh-index (aref vertex-mesh a))
                 (vector-push-extend a mesh-faces)
                 (vector-push-extend b mesh-faces)
                 (vector-push-extend c mesh-faces)))
          collect (cons mesh-vertices (make-array (length mesh-faces) :element-type '(unsigned-byte 32) :initial-contents mesh-faces)))))

(defun normalize (vertices indices &key (threshold 0.001) (center (vec 0 0 0)) (scale 1.0))
  ;; TODO: could probably do this inline by going over verts first, then faces and only copying once.
  (let ((tree (org.shirakumo.fraf.trial.space.kd-tree:make-kd-tree))
        (new-vertices (make-array 0 :element-type 'double-float :adjustable T :fill-pointer T))
        (new-indices (make-array 0 :element-type '(unsigned-byte 32) :adjustable T :fill-pointer T)))
    (flet ((vertex-idx (p)
             (let* ((p (nv* (nv- p center) scale))
                    (nearest (org.shirakumo.fraf.trial.space.kd-tree:kd-tree-nearest p tree :max-radius threshold)))
               (cond (nearest
                      (vertex-index-index nearest))
                     (T
                      (let ((index (truncate (length new-vertices) 3)))
                        (org.shirakumo.fraf.trial.space.kd-tree:kd-tree-insert
                         (make-vertex-index (vx p) (vy p) (vz p) index) tree)
                        (vector-push-extend (float (vx p) 0d0) new-vertices)
                        (vector-push-extend (float (vy p) 0d0) new-vertices)
                        (vector-push-extend (float (vz p) 0d0) new-vertices)
                        index))))))
      (loop for i from 0 below (length indices) by 3
            for p1 = (v vertices (aref indices (+ i 0)))
            for p2 = (v vertices (aref indices (+ i 1)))
            for p3 = (v vertices (aref indices (+ i 2)))
            for i1 = (vertex-idx p1)
            for i2 = (vertex-idx p2)
            for i3 = (vertex-idx p3)
            do (unless (or (= i1 i2) (= i1 i3) (= i2 i3))
                 (vector-push-extend i1 new-indices)
                 (vector-push-extend i2 new-indices)
                 (vector-push-extend i3 new-indices))))
    (values (replace (make-array (length new-vertices) :element-type 'double-float) new-vertices)
            (replace (make-array (length new-indices) :element-type '(unsigned-byte 32)) new-indices))))
