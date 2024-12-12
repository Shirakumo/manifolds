;;;; Based on https://github.com/hbf/miniball
(in-package #:org.shirakumo.fraf.manifolds)

;;; Subspan.cs
(defstruct (subspan (:constructor %make-subspan))
  (vertices (make-array 0 :element-type 'single-float) :type (vertex-array single-float))
  (membership (make-array 0 :element-type 'bit) :type simple-bit-vector)
  (members (make-array 4 :element-type 'vertex) :type (simple-array vertex (4)))
  (q (meye 3) :type mat3)
  (r (mat3) :type mat3)
  (u (vec3) :type vec3)
  (w (vec3) :type vec3)
  (rank 0 :type vertex)
  (c 0.0 :type single-float)
  (s 0.0 :type single-float))

(defmacro with-subspan ((subspan) &body body)
  `(let ((vertices (subspan-vertices ,subspan))
         (membership (subspan-membership ,subspan))
         (members (subspan-members ,subspan))
         (QQ (subspan-q ,subspan))
         (RR (subspan-r ,subspan))
         (u (subspan-u ,subspan))
         (w (subspan-w ,subspan)))
     (declare (ignorable vertices membership members QQ RR u w))
     (symbol-macrolet ((r (subspan-rank ,subspan))
                       (c (subspan-c ,subspan))
                       (s (subspan-s ,subspan)))
       (macrolet ((QQ (i j) `(mcref QQ ,i ,j))
                  (RR (i j) `(mcref RR ,i ,j))
                  (u (i) `(vref u ,i))
                  (w (i) `(vref w ,i)))
         ,@body))))

(defun make-subspan (vertices k &optional (subspan (%make-subspan)))
  (let ((membership (make-array (truncate (length vertices) 3) :element-type 'bit))
        (members (make-array 4 :element-type 'vertex)))
    (setf (aref membership k) 1)
    (setf (aref members 0) k)
    (setf (subspan-vertices subspan) vertices)
    (setf (subspan-membership subspan) membership)
    (setf (subspan-members subspan) members)
    subspan))

(declaim (inline subspan-origin subspan-any subspan-member-p subspan-size))
(defun subspan-origin (subspan)
  (the vec3 (v (subspan-vertices subspan) (aref (subspan-members subspan) (subspan-rank subspan)))))

(defun subspan-any (subspan)
  (the vec3 (v (subspan-vertices subspan) (aref (subspan-members subspan) (subspan-rank subspan)))))

(defun subspan-member-p (subspan vertex)
  (sbitp (subspan-membership subspan) vertex))

(defun subspan-size (subspan)
  (1+ (subspan-rank subspan)))

(defun subspan-givens (subspan a b)
  (declare (type single-float a b))
  (declare (optimize speed (safety 1)))
  (with-subspan (subspan)
    (cond ((= 0 b)
           (setf c 1.0)
           (setf s 0.0))
          ((< (abs a) (abs b))
           (let ((tt (/ a b)))
             (setf s (/ (sqrt (1+ (* tt tt)))))
             (setf c (* s tt))))
          (T
           (let ((tt (/ b a)))
             (setf c (/ (sqrt (1+ (* tt tt)))))
             (setf s (* c tt)))))))

(defun subspan-append-column (subspan)
  (declare (optimize speed (safety 1)))
  (with-subspan (subspan)
    (dotimes (i 3)
      (setf (RR r i) 0.0)
      (dotimes (k 3)
        (incf (RR r i) (* (QQ i k) (u k)))))
    (loop for j downfrom (1- 3) above r
          do (subspan-givens subspan (RR r (1- j)) (RR r j))
             (setf (RR r (1- j))
                   (+ (* c (RR r (1- j)))
                      (* s (RR r j))))
             (loop for i from 0 below 3
                   for a = (QQ (1- j) i)
                   for b = (QQ j i)
                   do (setf (QQ (1- j) i) (+ (* c a) (* s b)))
                      (setf (QQ j i) (- (* c b) (* s a)))))))

(defun subspan-add (subspan vertex)
  (declare (optimize speed (safety 1)))
  (with-subspan (subspan)
    (!v- u (v vertices vertex) (subspan-origin subspan))
    (subspan-append-column subspan)
    (setf (aref membership vertex) 1)
    (setf (aref members (1+ r)) (aref members r))
    (setf (aref members r) vertex)
    (incf r)))

(defun subspan-shortest-vector-to-span (subspan p w)
  (declare (type vec3 p w))
  (declare (optimize speed (safety 1)))
  (let ((QQ (subspan-q subspan)))
    (!v- w (subspan-origin subspan) p)
    (dotimes (j (subspan-rank subspan) (vsqrlength w))
      (let ((scale 0.0))
        (declare (type single-float scale))
        (dotimes (i 3)
          (incf scale (* (vref w i) (mcref QQ j i))))
        (dotimes (i 3)
          (decf (vref w i) (* scale (mcref QQ j i))))))))

(defun subspan-find-affine-coefficients (subspan center lambdas)
  (declare (type vec3 center))
  (declare (type (simple-array single-float (4)) lambdas))
  (declare (optimize speed (safety 1)))
  (with-subspan (subspan)
    (!v- u center (subspan-origin subspan))
    (dotimes (i 3)
      (setf (w i) 0.0)
      (dotimes (k 3)
        (incf (w i) (* (QQ i k) (u k)))))
    (let ((origin-lambda 1.0))
      (loop for j downfrom (1- r) to 0
            do (loop for k from (1+ j) below r
                     do (decf (w j) (* (aref lambdas k) (RR k j))))
               (let ((lj (/ (w j) (RR j j))))
                 (setf (aref lambdas j) lj)
                 (decf origin-lambda lj)))
      (setf (aref lambdas r) origin-lambda))))

(defun subspan-hessenberg-clear (subspan pos)
  (declare (type vertex pos))
  (declare (optimize speed (safety 1)))
  (with-subspan (subspan)
    (loop while (< pos r)
          do (subspan-givens subspan (RR pos pos) (RR pos (1+ pos)))
             (setf (RR pos pos) (+ (* c (RR pos pos)) (* s (RR pos (1+ pos)))))
             (loop for j from (1+ pos) below r
                   for a = (RR j pos)
                   for b = (RR j (1+ pos))
                   do (setf (RR j pos) (+ (* c a) (* s b)))
                      (setf (RR j (1+ pos)) (- (* c b) (* s a))))
             (loop for i from 0 below 3
                   for a = (QQ pos i)
                   for b = (QQ (1+ pos) i)
                   do (setf (QQ pos i) (+ (* c a) (* s b)))
                      (setf (QQ (1+ pos) i) (- (* c b) (* s a))))
             (incf pos))))

(defun subspan-special-rank-1-update (subspan)
  (declare (optimize speed (safety 1)))
  (with-subspan (subspan)
    (dotimes (i 3)
      (setf (w i) 0.0)
      (dotimes (k 3)
        (incf (w i) (* (qq i k) (u k)))))
    (loop for k downfrom 2 above 0
          do (subspan-givens subspan (w (1- k)) (w k))
             (setf (w (1- k)) (+ (* c (w (1- k))) (* s (w k))))
             (setf (RR (1- k) k) (* (- s) (RR (1- k) (1- k))))
             (setf (RR (1- k) (1- k)) (* c (RR (1- k) (1- k))))
             (loop for j from k below r
                   for a = (RR j (1- k))
                   for b = (RR j k)
                   do (setf (RR j (1- k)) (+ (* c a) (* s b)))
                      (setf (RR j k) (- (* c b) (* s a))))
             (loop for i from 0 below 3
                   for a = (qq (1- k) i)
                   for b = (qq k i)
                   do (setf (qq (1- k) i) (+ (* c a) (* s b)))
                      (setf (qq k i) (- (* c b) (* s a)))))
    (dotimes (j r)
      (incf (RR j 0) (w 0)))
    (subspan-hessenberg-clear subspan 0)))

(defun subspan-remove (subspan vertex)
  (declare (type vertex vertex))
  (declare (optimize speed (safety 1)))
  (with-subspan (subspan)
    (setf (aref membership (aref members vertex)) 0)
    (cond ((= vertex r)
           (!v- u (subspan-origin subspan) (v vertices (aref members (1- r))))
           (decf r)
           (subspan-special-rank-1-update subspan))
          (T
           (let ((dummy (mcol RR vertex)))
             (declare (type (simple-array single-float (3)) dummy))
             (loop for j from (1+ vertex) below r
                   do (setf (mcol RR (1- j)) (mcol RR j))
                      (setf (aref members (1- j)) (aref members j)))
             (setf (aref members (1- r)) (aref members r))
             (setf (mcol RR (decf r)) dummy))
           (subspan-hessenberg-clear subspan vertex)))))

;;; Miniball.cs
(defstruct (miniball (:include subspan))
  (center (vec3) :type vec3)
  (center-to-aff (vec3) :type vec3)
  (center-to-point (vec3) :type vec3)
  (lambdas (make-array 4 :element-type 'single-float) :type (simple-array single-float (4)))
  (dist-to-aff 0.0 :type single-float)
  (dist-to-aff-square 0.0 :type single-float)
  (squared-radius 0.0 :type single-float)
  (radius 0.0 :type single-float)
  (stopper 0 :type (signed-byte 31))
  (eps 1e-14 :type single-float))

(defmacro with-miniball ((miniball) &body body)
  `(let ((vertices (miniball-vertices ,miniball))
         (center (miniball-center ,miniball))
         (center-to-aff (miniball-center-to-aff ,miniball))
         (center-to-point (miniball-center-to-point ,miniball))
         (lambdas (miniball-lambdas ,miniball)))
     (declare (ignorable vertices center center-to-aff center-to-point lambdas))
     (symbol-macrolet ((dist-to-aff (miniball-dist-to-aff ,miniball))
                       (dist-to-aff-square (miniball-dist-to-aff-square ,miniball))
                       (squared-radius (miniball-squared-radius ,miniball))
                       (radius (miniball-radius ,miniball))
                       (stopper (miniball-stopper ,miniball))
                       (eps (miniball-eps ,miniball)))
       ,@body)))

(defun miniball-compute-dist-to-aff (miniball)
  (declare (optimize speed (safety 1)))
  (with-miniball (miniball)
    (setf dist-to-aff-square (subspan-shortest-vector-to-span miniball center center-to-aff))
    (setf dist-to-aff (sqrt (the (single-float 0.0) dist-to-aff-square)))))

(defun miniball-update-radius (miniball)
  (declare (optimize speed (safety 1)))
  (with-miniball (miniball)
    (setf squared-radius (vsqrdistance (subspan-any miniball) center))
    (setf radius (sqrt (the (single-float 0.0) squared-radius)))))

(defun miniball-successful-drop (miniball)
  (declare (optimize speed (safety 1)))
  (with-miniball (miniball)
    (subspan-find-affine-coefficients miniball center lambdas)
    (let ((smallest 0)
          (minimum 1.0))
      (dotimes (i (subspan-size miniball))
        (when (< (aref lambdas i) minimum)
          (setf minimum (aref lambdas i))
          (setf smallest i)))
      (when (<= minimum 0)
        (subspan-remove miniball smallest)
        T))))

(defun miniball-find-stop-fraction (miniball)
  (declare (optimize speed (safety 1)))
  (with-miniball (miniball)
    (let ((scale 1.0))
      (setf stopper -1)
      (dotimes (j (truncate (length vertices) 3) scale)
        (unless (subspan-member-p miniball j)
          (!v- center-to-point (v vertices j) center)
          (let ((dir-point-prod (v. center-to-aff center-to-point)))
            (unless (< (- dist-to-aff-square dir-point-prod) (* eps radius dist-to-aff))
              (let ((bound (/ (- squared-radius (vsqrlength center-to-point))
                              (* 2 (- dist-to-aff-square dir-point-prod)))))
                (when (and (< 0 bound) (< bound scale))
                  (setf scale bound)
                  (setf stopper j))))))))))

(defun bounding-sphere (vertices)
  (declare (optimize speed (safety 1)))
  (check-type vertices (vertex-array single-float))
  (if (= 0 (length vertices))
      (values (vec3) 0.0)
      (let ((center (v vertices 0))
            (squared-radius 0.0)
            (farthest 0))
        (loop for i from 1 below (truncate (length vertices) 3)
              for v = (v vertices i)
              for dist = (vsqrdistance center v)
              do (when (<= squared-radius dist)
                   (setf squared-radius dist)
                   (setf farthest i)))
        (let ((miniball (make-miniball :center center
                                       :squared-radius squared-radius
                                       :radius (sqrt squared-radius))))
          (make-subspan vertices farthest miniball)
          (with-miniball (miniball)
            (loop (miniball-compute-dist-to-aff miniball)
                  (loop while (<= dist-to-aff (* eps radius))
                        do (unless (miniball-successful-drop miniball)
                             (return))
                           (miniball-compute-dist-to-aff miniball))
                  (let ((scale (miniball-find-stop-fraction miniball)))
                    (declare (type single-float scale))
                    (cond ((<= 0 stopper)
                           (nv+* center center-to-aff scale)
                           (miniball-update-radius miniball)
                           (subspan-add miniball stopper))
                          (T
                           (nv+ center center-to-aff)
                           (miniball-update-radius miniball)
                           (unless (miniball-successful-drop miniball)
                             (return))))))
            (values center radius))))))
