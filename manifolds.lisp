(in-package #:org.shirakumo.fraf.manifolds)

(defmacro do-faces ((a b c faces &optional result) &body body)
  (let ((i (gensym "I"))
        (f (gensym "F")))
    `(loop with ,f of-type face-array = ,faces
           for ,i from 0 below (length ,f) by 3
           for ,a of-type vertex = (aref ,f (+ 0 ,i))
           for ,b of-type vertex = (aref ,f (+ 1 ,i))
           for ,c of-type vertex = (aref ,f (+ 2 ,i))
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
  (check-type faces face-array)
  (let ((adjacency (or adjacency
                       (make-array (1+ (loop for idx across faces maximize idx)) :initial-element ()))))
    (do-faces (a b c faces adjacency)
      (pushnew b (aref adjacency a))
      (pushnew c (aref adjacency a))
      (pushnew a (aref adjacency b))
      (pushnew c (aref adjacency b))
      (pushnew a (aref adjacency c))
      (pushnew b (aref adjacency c)))))

(defun face-adjacency-list (faces &optional adjacency)
  (check-type faces face-array)
  (let ((table (make-hash-table :test 'eql))
        (adjacency (or adjacency
                       (make-array (truncate (length faces) 3) :initial-element ())))
        (face 0))
    (flet ((edge (a b face)
             (when (< b a)
               (rotatef a b))
             (let ((edge (+ (ash a 32) b)))
               (push face (gethash edge table)))))
      (do-faces (a b c faces)
        (edge a b face)
        (edge b c face)
        (edge c a face)
        (incf face)))
    (loop for adjacents being the hash-values of table
          do (loop for face in adjacents
                   do (loop for other in adjacents
                            unless (= other face)
                            do (push other (aref adjacency face)))))
    adjacency))

(defun half-edge-list (faces)
  (check-type faces face-array)
  (let ((edges (make-array (length faces) :element-type T))
        (i 0))
    (flet ((add-edge (a b)
             (setf (aref edges i) (edge a b))
             (incf i)))
      (do-faces (a b c faces edges)
        (add-edge a b)
        (add-edge b c)
        (add-edge c a)))))

(defun edge-list (faces)
  (check-type faces face-array)
  (let ((edge-table (make-hash-table :test 'eql))
        (edge-count 0))
    (flet ((add-edge (a b)
             (if (< a b)
                 (let ((existing (gethash a edge-table)))
                   (unless (member b existing)
                     (setf (gethash a edge-table) (list* b existing))
                     (incf edge-count)))
                 (let ((existing (gethash b edge-table)))
                   (unless (member a existing)
                     (setf (gethash b edge-table) (list* a existing))
                     (incf edge-count))))))
      (do-faces (a b c faces)
        (add-edge a b)
        (add-edge b c)
        (add-edge c a)))
    (loop with edges = (make-array edge-count)
          with i = 0
          for a being the hash-keys of edge-table using (hash-value bs)
          do (dolist (b bs)
               (setf (aref edges i) (edge a b))
               (incf i))
          finally (return edges))))

(declaim (inline face-corner))
(defun face-corner (face a b faces)
  (check-type faces face-array)
  (let ((i0 (aref faces (+ (* 3 face) 0)))
        (i1 (aref faces (+ (* 3 face) 1)))
        (i2 (aref faces (+ (* 3 face) 2))))
    (cond ((= i0 a) (if (= i1 b) i2 i1))
          ((= i1 a) (if (= i0 b) i2 i0))
          ((= i2 a) (if (= i0 b) i1 i0))
          (T (error "Edge ~a ~a is not part of face ~a!" a b face)))))

(defun adjacent-faces (face a b faces &optional adjacency)
  (unless adjacency (setf adjacency (face-adjacency-list faces)))
  (loop for adjacent in (aref adjacency face)
        for i0 = (aref faces (+ (* 3 adjacent) 0))
        for i1 = (aref faces (+ (* 3 adjacent) 1))
        for i2 = (aref faces (+ (* 3 adjacent) 2))
        when (or (and (= i0 a) (or (= i1 b) (= i2 b)))
                 (and (= i1 a) (or (= i0 b) (= i2 b)))
                 (and (= i2 a) (or (= i0 b) (= i1 b))))
        collect adjacent))

(defun edge-adjacency-map (faces &optional adjacency)
  (check-type faces face-array)
  (unless adjacency (setf adjacency (face-adjacency-list faces)))
  (let ((edges (make-array (length faces))))
    (flet ((map-edge (tri a b)
             (setf (aref edges tri) (adjacent-faces tri a b faces adjacency))))
      (loop for i from 0 below (length faces) by 3
            for tri from 0
            do (map-edge tri (aref faces (+ i 0)) (aref faces (+ i 1)))
               (map-edge tri (aref faces (+ i 1)) (aref faces (+ i 2)))
               (map-edge tri (aref faces (+ i 2)) (aref faces (+ i 0)))))
    edges))

(defun boundary-list (faces)
  (check-type faces face-array)
  (let ((edge-table (make-hash-table :test 'eql))
        (edge-count 0))
    (flet ((add-edge (a b c)
             (multiple-value-bind (edge flippedp)
                 (if (< b a)
                     (values (+ (ash b 32) a) t)
                     (values (+ (ash a 32) b) nil))
               (setf (gethash edge edge-table)
                     (case (gethash edge edge-table)
                       (:inner :inner) ; > 2 incident faces
                       ((nil) ; first incident face
                        (incf edge-count)
                        ;; Store index of third vertex and encode
                        ;; winding direction in sign.
                        (if flippedp (- c) c))
                       (t ; second incident face
                        (decf edge-count)
                        :inner))))))
      (do-faces (a b c faces)
        (add-edge a b c)
        (add-edge b c a)
        (add-edge c a b)))
    (loop with edges = (make-array edge-count)
          with i = 0
          for edge being the hash-keys of edge-table using (hash-value info)
          unless (eq info :inner)
            do (let* ((flippedp (minusp info))
                      (a (ldb (byte 32 32) edge))
                      (b (ldb (byte 32 0) edge))
                      ;; Restore winding direction and index of third
                      ;; vertex.
                      (edge (if flippedp
                                (extended-edge b a (- info))
                                (extended-edge a b info))))
                 (setf (aref edges i) edge)
                 (incf i))
          finally (return edges))))

(declaim (inline v (setf v) sbitp (setf sbitp)))
(defun v (vertices i &optional vec)
  (check-type vertices vertex-array)
  (check-type i face)
  (let ((i (* 3 i)))
    (etypecase vec
      (null
       (etypecase vertices
         ((vertex-array single-float)
          (vec (aref vertices (+ 0 i))
               (aref vertices (+ 1 i))
               (aref vertices (+ 2 i))))
         ((vertex-array double-float)
          (dvec (aref vertices (+ 0 i))
                (aref vertices (+ 1 i))
                (aref vertices (+ 2 i))))))
      ((or vec3 dvec3)
       (etypecase vertices
         ((vertex-array single-float)
          (vsetf vec (aref vertices (+ 0 i))
                 (aref vertices (+ 1 i))
                 (aref vertices (+ 2 i))))
         ((vertex-array double-float)
          (vsetf vec (aref vertices (+ 0 i))
                 (aref vertices (+ 1 i))
                 (aref vertices (+ 2 i)))))))))

(defun (setf v) (v vertices i)
  (check-type i face)
  (let ((i (* 3 i)))
    (etypecase vertices
      ((vertex-array single-float)
       (setf (aref vertices (+ 0 i)) (vx v))
       (setf (aref vertices (+ 1 i)) (vy v))
       (setf (aref vertices (+ 2 i)) (vz v)))
      ((vertex-array double-float)
       (setf (aref vertices (+ 0 i)) (float (vx v) 0d0))
       (setf (aref vertices (+ 1 i)) (float (vy v) 0d0))
       (setf (aref vertices (+ 2 i)) (float (vz v) 0d0)))
      ((array single-float)
       (setf (aref vertices (+ 0 i)) (vx v))
       (setf (aref vertices (+ 1 i)) (vy v))
       (setf (aref vertices (+ 2 i)) (vz v)))
      ((array double-float)
       (setf (aref vertices (+ 0 i)) (float (vx v) 0d0))
       (setf (aref vertices (+ 1 i)) (float (vy v) 0d0))
       (setf (aref vertices (+ 2 i)) (float (vz v) 0d0))))
    v))

(defun sbitp (array i)
  (= 1 (sbit array i)))

(defun (setf sbitp) (value array i)
  (setf (sbit array i) (if value 1 0)))

(defun vertex-normal (vertices vertex adjacents)
  (check-type vertices vertex-array)
  (check-type vertex vertex)
  (check-type adjacents list)
  (let ((normal (vec3))
        (count 0))
    (flet ((normal (v1 v2)
             (let* ((a (v vertices vertex))
                    (b (v vertices v1))
                    (c (v vertices v2)))
               (incf count)
               (nvunit (vc (v- b a) (v- c a))))))
      (loop for (a b) on adjacents
            do (if b
                   (nv+ normal (normal a b))
                   (nv+ normal (normal a (first adjacents)))))
      (nv* normal (/ count)))))

(defun face-normal (vertices faces face)
  (check-type vertices vertex-array)
  (check-type faces face-array)
  (check-type face face)
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
  (check-type vertices vertex-array)
  (check-type faces face-array)
  (dotimes (i (length face-normals) face-normals)
    (setf (aref face-normals i) (face-normal vertices faces i))))

(defun face-normals* (vertices faces &optional (face-normals (make-array (truncate (length faces) 3))))
  (dotimes (i (length face-normals) face-normals)
    (setf (aref face-normals i) (face-normal* vertices faces i))))

(declaim (inline triangle-area))
(defun triangle-area (p1 p2 p3)
  (let* ((p2-p1 (v- p2 p1))
         (base (v2norm p2-p1)))
    (declare (dynamic-extent p2-p1))
    (if (= 0 base)
        base
        (let ((p3-p1 (v- p3 p1)))
          (declare (dynamic-extent p3-p1))
          (* 0.5 base (v2norm (nv- p3-p1 (nv* p2-p1 (/ (v. p3-p1 p2-p1) (* base base))))))))))

(defun face-area (vertices faces face)
  (check-type vertices vertex-array)
  (check-type faces face-array)
  (check-type face face)
  (with-face-specialization (faces)
    (with-vertex-specialization (vertices)
      (let* ((i (* 3 face))
             (p1 (v vertices (aref faces (+ 0 i))))
             (p2 (v vertices (aref faces (+ 1 i))))
             (p3 (v vertices (aref faces (+ 2 i)))))
        (let* ((p2-p1 (v- p2 p1))
               (base (v2norm p2-p1))
               (height (if (= 0 base)
                           (coerce 0 vertex-component-type)
                           (let ((p3-p1 (v- p3 p1)))
                             (v2norm (v- p3-p1 (v* p2-p1
                                                   (/ (v. p3-p1 p2-p1)
                                                      (* base base)))))))))
          (* (coerce 0.5 vertex-component-type) base height))))))

(defun surface-area (vertices faces)
  (check-type vertices vertex-array)
  (check-type faces face-array)
  (loop for face from 0 below (truncate (length faces) 3)
        sum (face-area vertices faces face)))

(defun boundary-length (vertices faces)
  (check-type vertices vertex-array)
  (check-type faces face-array)
  (loop for edge across (boundary-list faces)
        for a = (start edge)
        for b = (end edge)
        sum (vdistance (v vertices a) (v vertices b))))

(defun centroid (vertices faces)
  (check-type vertices vertex-array)
  (check-type faces face-array)
  (with-vertex-specialization (vertices)
    (with-face-specialization (faces)
      (let ((numerator (ecase vertex-component-type
                         (single-float (vec3))
                         (double-float (dvec3))))
            (denominator (coerce 0 vertex-component-type)))
        (loop for i from 0 below (length faces) by 3
              for i1 = (aref faces (+ 0 i))
              for i2 = (aref faces (+ 1 i))
              for i3 = (aref faces (+ 2 i))
              for sum = (v/ (v+ (v vertices i1) (v vertices i2) (v vertices i3)) 3)
              for area = (face-area vertices faces (truncate i 3))
              do (nv+ numerator (v* sum area))
                 (incf denominator area))
        (if (= denominator 0)
            numerator
            (nv* numerator (/ denominator)))))))

;;; Algorithm described in “Efficient feature extraction for 2D/3D
;;; objects in mesh representation” by Cha Zhang and Tsuhan Chen (2001) [1,2].
;;;
;;; [1]: https://doi.org/10.1109/ICIP.2001.958278
;;; [2]: http://chenlab.ece.cornell.edu/Publication/Cha/icip01_Cha.pdf
(defun volume (vertices faces)
  (check-type vertices vertex-array)
  (check-type faces face-array)
  (with-vertex-specialization (vertices)
    (/ (loop for i from 0 below (length faces) by 3
             for p1 = (v vertices (aref faces (+ 0 i)))
             for p2 = (v vertices (aref faces (+ 1 i)))
             for p3 = (v vertices (aref faces (+ 2 i)))
             sum (+ (- (* (vx p3) (vy p2) (vz p1)))
                    (+ (* (vx p2) (vy p3) (vz p1)))
                    (+ (* (vx p3) (vy p1) (vz p2)))
                    (- (* (vx p1) (vy p3) (vz p2)))
                    (- (* (vx p2) (vy p1) (vz p3)))
                    (+ (* (vx p1) (vy p2) (vz p3)))))
       6)))

(defun closest-point-on-triangle (vertices faces face point)
  (check-type vertices vertex-array)
  (check-type faces face-array)
  (check-type face face)
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

(defun ray-triangle (p dir a b c)
  (let* ((ab (v- b a))
         (ac (v- c a))
         (ap (v- p a))
         (n (vc ab ac))
         (d (- (v. dir n)))
         (ood (/ d))
         (tt (* ood (v. ap n))))
    (when (<= 0 tt)
      (let* ((e (v- (vc dir ap)))
             (v (* (v. ac e) ood)))
        (when (<= 0 v 1)
          (let ((w (- (* (v. ab e) ood))))
            (when (and (<= 0 w) (<= (+ v w) 1))
              (let ((u (- 1 v w)))
                (values t u v w d n)))))))))

(defun face-in-volume-p (vertices faces face location bsize)
  (declare (type vec3 location bsize))
  (check-type vertices vertex-array)
  (check-type faces face-array)
  (check-type face face)
  ;; SAT
  (let* ((v0 (v vertices (aref faces (+ 0 (* face 3)))))
         (v1 (v vertices (aref faces (+ 1 (* face 3)))))
         (v2 (v vertices (aref faces (+ 2 (* face 3)))))
         (v0 (v- v0 location)) (v1 (v- v1 location)) (v2 (v- v2 location))
         (f0 (v- v1 v0)) (f1 (v- v2 v1)) (f2 (v- v0 v2)))
    (flet ((test-axis (ax ay az r)
             (let ((p0 (+ (* (vx v0) ax) (* (vy v0) ay) (* (vz v0) az)))
                   (p1 (+ (* (vx v1) ax) (* (vy v1) ay) (* (vz v1) az)))
                   (p2 (+ (* (vx v2) ax) (* (vy v2) ay) (* (vz v2) az))))
               (< r (max (- (max p0 p1 p2)) (min p0 p1 p2))))))
      (declare (inline test-axis))
      (not (or (test-axis 0 (- (vz f0)) (+ (vy f0)) (+ (* (vy bsize) (abs (vz f0))) (* (vz bsize) (abs (vy f0)))))
               (test-axis 0 (- (vz f1)) (+ (vy f1)) (+ (* (vy bsize) (abs (vz f1))) (* (vz bsize) (abs (vy f1)))))
               (test-axis 0 (- (vz f2)) (+ (vy f2)) (+ (* (vy bsize) (abs (vz f2))) (* (vz bsize) (abs (vy f2)))))
               (test-axis (+ (vz f0)) 0 (- (vx f0)) (+ (* (vx bsize) (abs (vz f0))) (* (vz bsize) (abs (vx f0)))))
               (test-axis (+ (vz f1)) 0 (- (vx f1)) (+ (* (vx bsize) (abs (vz f1))) (* (vz bsize) (abs (vx f1)))))
               (test-axis (+ (vz f2)) 0 (- (vx f2)) (+ (* (vx bsize) (abs (vz f2))) (* (vz bsize) (abs (vx f2)))))
               (test-axis (- (vy f0)) (+ (vx f0)) 0 (+ (* (vx bsize) (abs (vy f0))) (* (vy bsize) (abs (vx f0)))))
               (test-axis (- (vy f1)) (+ (vx f1)) 0 (+ (* (vx bsize) (abs (vy f1))) (* (vy bsize) (abs (vx f1)))))
               (test-axis (- (vy f2)) (+ (vx f2)) 0 (+ (* (vx bsize) (abs (vy f2))) (* (vy bsize) (abs (vx f2)))))
               (< (vx bsize) (min (vx v0) (vx v1) (vx v2)))
               (< (vy bsize) (min (vy v0) (vy v1) (vy v2)))
               (or (< (max (vz v0) (vz v1) (vz v2)) (- (vz bsize)))
                   (< (vz bsize) (min (vz v0) (vz v1) (vz v2))))
               (let ((n (vc f0 f1)))
                 (< (+ (* (vx bsize) (abs (vx n)))
                       (* (vy bsize) (abs (vy n)))
                       (* (vz bsize) (abs (vz n))))
                    (v. n v0))))))))

(defun intersects-volume-p (vertices faces location bsize)
  ;; FIXME: check if bsize entirely within mesh
  (dotimes (face (truncate faces 3) NIL)
    (when (face-in-volume-p vertices faces face location bsize)
      (return T))))

(defun faces-in-volume (vertices faces location bsize)
  (check-type vertices vertex-array)
  (check-type faces face-array)
  (let ((new-faces (make-array 0 :element-type '(unsigned-byte 32) :adjustable T :fill-pointer T)))
    (loop for face from 0 below (truncate (length faces) 3)
          for j from 0 by 3
          do (when (face-in-volume-p vertices faces face location bsize)
               (vector-push-extend (aref faces (+ 0 j)) new-faces)
               (vector-push-extend (aref faces (+ 1 j)) new-faces)
               (vector-push-extend (aref faces (+ 2 j)) new-faces)))
    (make-array (length new-faces) :element-type '(unsigned-byte 32) :initial-contents new-faces)))

(defun bounding-box (vertices)
  (check-type vertices vertex-array)
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
    (let ((bsize (nv* (nv- max min) 0.5)))
      (values (nv+ min bsize) bsize))))

(defun vertex-faces (faces &optional vertex-faces)
  (check-type faces face-array)
  (let ((vertex-faces (or vertex-faces
                          (make-array (1+ (loop for vertex across faces maximize vertex))))))
    (dotimes (i (length vertex-faces))
      (setf (aref vertex-faces i) (make-array 0 :adjustable T :fill-pointer T)))
    (dotimes (i (length faces) vertex-faces)
      (vector-push-extend (truncate i 3) (aref vertex-faces (aref faces i))))))

(defun 2-manifold-p (faces &optional adjacency)
  (check-type faces face-array)
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
  (check-type vertices vertex-array)
  (check-type faces face-array)
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

(defun transform-mesh (vertices matrix)
  (etypecase vertices
    ((vertex-array f32)
     (loop for i from 0 below (length vertices) by 3
           do (let ((v (vec (aref vertices (+ i 0))
                            (aref vertices (+ i 1))
                            (aref vertices (+ i 2)))))
                (declare (dynamic-extent v))
                (n*m matrix v)
                (setf (aref vertices (+ i 0)) (vx v)
                      (aref vertices (+ i 1)) (vy v)
                      (aref vertices (+ i 2)) (vz v)))))
    ((vertex-array f64)
     (loop for i from 0 below (length vertices) by 3
           do (let ((v (dvec (aref vertices (+ i 0))
                             (aref vertices (+ i 1))
                             (aref vertices (+ i 2)))))
                (declare (dynamic-extent v))
                (n*m matrix v)
                (setf (aref vertices (+ i 0)) (vx v)
                      (aref vertices (+ i 1)) (vy v)
                      (aref vertices (+ i 2)) (vz v))))))
  vertices)

(defun voxelize (vertices faces &key (grid 1.0))
  (multiple-value-bind (center bsize) (bounding-box vertices)
    (let ((grid (etypecase grid
                  ((integer 1)
                   (make-array (list grid grid grid) :element-type 'bit))
                  (single-float
                   (make-array (list (ceiling (/ (* 2 (vz bsize)) grid))
                                     (ceiling (/ (* 2 (vy bsize)) grid))
                                     (ceiling (/ (* 2 (vx bsize)) grid)))
                               :element-type 'bit))
                  (list
                   (destructuring-bind (x y z) grid
                     (make-array (list z y x) :element-type 'bit)))
                  ((simple-array bit (* * *))
                   grid))))
      (destructuring-bind (vd vh vw) (array-dimensions grid)
        (let ((s (vec (/ (vz bsize) vd)
                      (/ (vy bsize) vh)
                      (/ (vx bsize) vw)))
              (c (v- center bsize)))
          ;; Compute the shell
          (dotimes (i vd)
            (dotimes (j vh)
              (dotimes (k vw)
                ;; FIXME: this is obviously horribly inefficient
                (setf (aref grid i j k) (if (intersects-volume-p vertices faces c s) 1 0))
                (incf (vx c) (vx s)))
              (setf (vx c) (- (vx center) (vx bsize)))
              (incf (vy c) (vy s)))
            (setf (vy c) (- (vy center) (vy bsize)))
            (incf (vz c) (vz s)))
          (values grid center bsize))))))

(defun random-point-on-surface (vertices faces &optional (vec (vec3)))
  (declare (type vec3 vec))
  (declare (optimize speed))
  (check-type faces face-array)
  (with-vertex-specialization (vertices)
    (let* ((tri (* 3 (truncate (random (length faces)) 3)))
           (i0 (* 3 (aref faces (+ tri 0))))
           (i1 (* 3 (aref faces (+ tri 1))))
           (i2 (* 3 (aref faces (+ tri 2))))
           (p0 (vec (aref vertices (+ i0 0)) (aref vertices (+ i0 1)) (aref vertices (+ i0 2))))
           (p1 (vec (aref vertices (+ i1 0)) (aref vertices (+ i1 1)) (aref vertices (+ i1 2))))
           (p2 (vec (aref vertices (+ i2 0)) (aref vertices (+ i2 1)) (aref vertices (+ i2 2))))
           (f (random 1.0))
           (g (random 1.0)))
      (declare (dynamic-extent p0 p1 p2))
      (declare (type single-float f g))
      ;; Pick a random location on the triangle via barycentric interpolation
      (when (< 1 (+ f g))
        (setf f (- 1.0 f) g (- 1.0 g)))
      (let ((ba (v- p1 p0))
            (ca (v- p2 p0)))
        (declare (dynamic-extent ba ca))
        (v<- vec p0)
        (nv+* vec ba f)
        (nv+* vec ca g)))))

(defun random-point-in-volume (vertices faces &optional (vec (vec3)))
  (declare (type vec3 vec))
  (declare (optimize speed))
  (check-type faces face-array)
  (with-vertex-specialization (vertices)
    (multiple-value-bind (center bsize) (bounding-box vertices)
      (declare (type vec3 center bsize))
      ;; Perform simple rejection sampling
      (loop (!vrand vec center bsize)
            (when (point-in-volume-p vec vertices faces)
              (return vec))))))
