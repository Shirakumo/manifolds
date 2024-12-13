;;;; Based on https://github.com/hbf/miniball by Kaspar Fisher
(in-package #:org.shirakumo.fraf.manifolds)

(defstruct (subspan (:constructor %make-subspan))
  (vertices (make-array 0 :element-type 'single-float) :type vertex-array)
  (membership (make-array 0 :element-type 'bit) :type simple-bit-vector)
  (members (make-array 4 :element-type 'vertex :initial-element 0) :type (simple-array vertex (4)))
  (q (meye (dmat3)) :type dmat3)
  (r (dmat3) :type dmat3)
  (u (dvec3) :type dvec3)
  (w (dvec3) :type dvec3)
  (rank 0 :type vertex)
  (c 0d0 :type double-float)
  (s 0d0 :type double-float))

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
  (let ((membership (make-array (truncate (length vertices) 3) :element-type 'bit :initial-element 0)))
    (setf (sbitp membership k) T)
    (setf (aref (subspan-members subspan) 0) k)
    (setf (subspan-vertices subspan) vertices)
    (setf (subspan-membership subspan) membership)
    subspan))

(declaim (inline subspan-origin subspan-any subspan-member-p subspan-size subspan-givens))
(defun subspan-origin (subspan &optional (out (dvec3)))
  (the dvec3 (v (subspan-vertices subspan)
                (aref (subspan-members subspan) (subspan-rank subspan))
                out)))

(defun subspan-any (subspan &optional (out (dvec3)))
  (the dvec3 (v (subspan-vertices subspan)
                (aref (subspan-members subspan) (subspan-rank subspan))
                out)))

(defun subspan-member-p (subspan vertex)
  (sbitp (subspan-membership subspan) vertex))

(defun subspan-size (subspan)
  (1+ (subspan-rank subspan)))

(defun subspan-givens (subspan a b)
  (declare (type double-float a b))
  (declare (optimize speed (safety 1)))
  (with-subspan (subspan)
    (cond ((= 0 b)
           (setf c 1d0)
           (setf s 0d0))
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
      (setf (RR r i) 0d0)
      (dotimes (k 3)
        (incf (RR r i) (* (QQ i k) (u k)))))
    (loop for j downfrom (1- 3) above r
          do (subspan-givens subspan (RR r (1- j)) (RR r j))
             (setf (RR r (1- j)) (+ (* c (RR r (1- j))) (* s (RR r j))))
             (loop for i from 0 below 3
                   for a = (QQ (1- j) i)
                   for b = (QQ j i)
                   do (setf (QQ (1- j) i) (+ (* c a) (* s b)))
                      (setf (QQ j i) (- (* c b) (* s a)))))))

(defun subspan-add (subspan vertex)
  (declare (optimize speed (safety 1)))
  (with-subspan (subspan)
    (!v- u (v vertices vertex u) (subspan-origin subspan))
    (subspan-append-column subspan)
    (setf (sbitp membership vertex) T)
    (setf (aref members (1+ r)) (aref members r))
    (setf (aref members r) vertex)
    (incf r)))

(defun subspan-shortest-vector-to-span (subspan p w)
  (declare (type dvec3 p w))
  (declare (optimize speed (safety 1)))
  (let ((QQ (subspan-q subspan)))
    (!v- w (subspan-origin subspan w) p)
    (dotimes (j (subspan-rank subspan) (vsqrlength w))
      (let ((scale 0d0))
        (declare (type double-float scale))
        (dotimes (i 3)
          (incf scale (* (vref w i) (mcref QQ j i))))
        (dotimes (i 3)
          (decf (vref w i) (* scale (mcref QQ j i))))))))

(defun subspan-find-affine-coefficients (subspan center lambdas)
  (declare (type dvec3 center))
  (declare (type (simple-array double-float (4)) lambdas))
  (declare (optimize speed (safety 1)))
  (with-subspan (subspan)
    (!v- u center (subspan-origin subspan u))
    (dotimes (i 3)
      (setf (w i) 0d0)
      (dotimes (k 3)
        (incf (w i) (* (QQ i k) (u k)))))
    (let ((origin-lambda 1d0))
      (declare (type double-float origin-lambda))
      (loop for j downfrom (1- r) to 0
            do (loop for k from (1+ j) below r
                     do (decf (w j) (* (aref lambdas k) (RR k j))))
               (when (/= 0 (RR j j))
                 ;; KLUDGE: Can get div by zero here, not sure what to do about it.
                 (let ((lj (/ (w j) (RR j j))))
                   (setf (aref lambdas j) lj)
                   (decf origin-lambda lj))))
      (setf (aref lambdas r) origin-lambda)
      NIL)))

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
      (setf (w i) 0d0)
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
    (setf (sbitp membership (aref members vertex)) NIL)
    (cond ((= vertex r)
           (!v- u (subspan-origin subspan) (v vertices (aref members (1- r)) u))
           (decf r)
           (subspan-special-rank-1-update subspan))
          (T
           (let ((dummy (mcol RR vertex)))
             (declare (type (simple-array double-float (3)) dummy))
             (loop for j from (1+ vertex) below r
                   do (setf (mcol RR (1- j)) (mcol RR j))
                      (setf (aref members (1- j)) (aref members j)))
             (setf (aref members (1- r)) (aref members r))
             (setf (mcol RR (decf r)) dummy))
           (subspan-hessenberg-clear subspan vertex)))))

(defun bounding-sphere (vertices)
  (declare (optimize speed (safety 1)))
  (check-type vertices vertex-array)
  (flet ((emit (center radius)
           (if (eql (array-element-type vertices) 'single-float)
               (values (vec3 center) (float radius 0f0))
               (values center radius))))
    (if (= 0 (length vertices))
        (emit (dvec3) 0d0)
        (let ((center (dvec3))
              (tmp (dvec3))
              (squared-radius 0d0)
              (farthest 0))
          (v vertices 0 center)
          (loop for i from 1 below (truncate (length vertices) 3)
                for dist = (vsqrdistance center (v vertices i tmp))
                do (when (<= squared-radius dist)
                     (setf squared-radius dist)
                     (setf farthest i)))
          ;; Actual miniball update loop
          (let ((subspan (make-subspan vertices farthest))
                (radius (sqrt squared-radius))
                (center-to-aff (dvec3))
                (center-to-point (dvec3))
                (lambdas (make-array 4 :element-type 'double-float :initial-element 0d0))
                (dist-to-aff 0d0)
                (dist-to-aff-square 0d0)
                (eps 1d-12))
            (declare (type double-float dist-to-aff dist-to-aff-square eps))
            (declare (dynamic-extent subspan center-to-aff center-to-point lambdas))
            (flet ((update-radius ()
                     (setf squared-radius (vsqrdistance (subspan-any subspan tmp) center))
                     (setf radius (sqrt (the (double-float 0d0) squared-radius))))
                   (compute-dist-to-aff ()
                     (setf dist-to-aff-square (subspan-shortest-vector-to-span subspan center center-to-aff))
                     (setf dist-to-aff (sqrt (the (double-float 0d0) dist-to-aff-square))))
                   (find-stop-fraction ()
                     (let ((scale 1d0) stopper)
                       (dotimes (j (truncate (length vertices) 3) (values scale stopper))
                         (unless (subspan-member-p subspan j)
                           (!v- center-to-point (v vertices j tmp) center)
                           (let ((dir-point-prod (v. center-to-aff center-to-point)))
                             (unless (< (- dist-to-aff-square dir-point-prod) (* eps radius dist-to-aff))
                               (let ((a (- squared-radius (vsqrlength center-to-point)))
                                     (b (* 2 (- dist-to-aff-square dir-point-prod))))
                                 (when (< 0 b)
                                   (let ((bound (/ a b)))
                                     (when (< 0 bound scale)
                                       (setf scale bound)
                                       (setf stopper j)))))))))))
                   (successful-drop ()
                     (subspan-find-affine-coefficients subspan center lambdas)
                     (let ((smallest 0)
                           (minimum 1d0))
                       (dotimes (i (subspan-size subspan))
                         (when (< (aref lambdas i) minimum)
                           (setf minimum (aref lambdas i))
                           (setf smallest i)))
                       (when (<= minimum 0)
                         (subspan-remove subspan smallest)
                         T))))
              (loop (compute-dist-to-aff)
                    (loop while (<= dist-to-aff (* eps radius))
                          do (unless (successful-drop)
                               (return))
                             (compute-dist-to-aff))
                    (multiple-value-bind (scale stopper) (find-stop-fraction)
                      (declare (type double-float scale))
                      (cond (stopper
                             (nv+* center center-to-aff scale)
                             (update-radius)
                             (subspan-add subspan stopper))
                            (T
                             (nv+ center center-to-aff)
                             (update-radius)
                             (unless (successful-drop)
                               (return)))))))
            (emit center radius))))))
