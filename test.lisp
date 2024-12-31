(defpackage #:org.shirakumo.fraf.manifolds.test
  (:nicknames #:manifolds-test)
  (:import-from #:org.shirakumo.fraf.math.internal #:~=)
  (:use
   #:cl
   #:parachute
   #:org.shirakumo.fraf.math
   #:org.shirakumo.fraf.manifolds)
  (:local-nicknames
   (#:wavefront #:org.shirakumo.fraf.wavefront))
  (:export
   #:normalize-file))

(in-package #:org.shirakumo.fraf.manifolds.test)

(defvar *test-directory*
  (merge-pathnames
   (make-pathname :directory '(:relative "test"))
   (make-pathname :name nil :type nil :defaults #.(or *compile-file-pathname*
                                                      *load-pathname*))))

(defun test-file (name)
  (merge-pathnames name *test-directory*))

(defun load-mesh (name)
  (let* ((filename (test-file (if (stringp name) (make-pathname :name name :type "obj") name)))
         (context (wavefront:parse filename))
         (meshes (wavefront:extract-meshes context NIL '(:position))))
    (assert (= (length meshes) 1))
    (assert (= (wavefront:face-length (first meshes)) 3))
    (first meshes)))

(defun save-mesh (file vertices faces)
  (wavefront:serialize (make-instance 'wavefront:mesh
                                      :name "Mesh"
                                      :vertex-data vertices
                                      :index-data faces
                                      :attributes '(:position))
                       file :if-exists :supersede)
  file)

(defun vertex-array (component-type components)
  (map-into (make-array (length components) :element-type component-type)
            (lambda (value) (coerce value component-type))
            components))

(defun face-array (indices)
  (make-array 3 :element-type 'u32 :initial-contents indices))

(defun load-vertices (name &optional (precision 'double-float))
  (let* ((filename (test-file (make-pathname :name name :type "obj")))
         (context (wavefront:parse filename (make-instance 'wavefront:context :precision precision)))
         (src (wavefront:vertices context))
         (dst (make-array (* 3/4 (length src)) :element-type precision)))
    (loop for i from 0 below (length dst) by 3
          for j from 0 below (length src) by 4
          do (replace dst src :start1 i :start2 j :end1 (+ i 3)))
    dst))

(define-test edge-list.smoke
  (let* ((mesh (load-mesh "box"))
         (edge-list (edge-list (wavefront:index-data mesh))))
    (is eql 18 (length edge-list))))

(define-test face-area.smoke
  (flet ((test (vertex-component-type)
           (let* ((vertices (vertex-array
                             vertex-component-type
                             '(0 0 0 0 1 0 1 0 0)))
                  (faces (face-array '(0 1 2)))
                  (area (face-area vertices faces 0)))
             (true (typep area vertex-component-type))
             (is = (coerce 1/2 vertex-component-type) area))))
    (test 'single-float)
    (test 'double-float)))

(define-test boundary-list.smoke
  (let* ((mesh (load-mesh "box"))
         (boundary-list (boundary-list (wavefront:index-data mesh))))
    (is eql 0 (length boundary-list)))

  (let* ((mesh (load-mesh "boundary-1"))
         (boundary-list (boundary-list (wavefront:index-data mesh))))
    (is eql 3 (length boundary-list))
    (map nil (lambda (element)
               (of-type 'extended-edge element))
         (boundary-list (wavefront:index-data mesh))))

  (let* ((mesh (load-mesh "boundary-2"))
         (boundary-list (boundary-list (wavefront:index-data mesh))))
    (is eql 8 (length boundary-list))
    (map nil (lambda (element)
               (of-type 'extended-edge element))
         (boundary-list (wavefront:index-data mesh)))))

(defun normalize-file (in out &rest args)
  (let ((mesh (load-mesh in)))
    (multiple-value-bind (v f) (apply #'normalize (wavefront:vertex-data mesh) (wavefront:index-data mesh) args)
      (save-mesh (test-file (if (stringp out) (make-pathname :name out :type "obj") out)) v f))))

(define-test normalize.smoke
  (flet ((test (vertex-component-type)
           (let* ((vertices (vertex-array vertex-component-type
                                          '(0 0 0 1 0 0 0 1 0)))
                  (faces    (face-array '(0 1 2))))
             (multiple-value-bind (result-vertices result-faces)
                 (normalize vertices faces)
               (true (typep result-vertices 'vertex-array))
               (true (subtypep (array-element-type result-vertices)
                               (upgraded-array-element-type vertex-component-type)))
               (true (typep result-faces 'face-array))))))
    (test 'single-float)
    (test 'double-float))

  (dolist (file (directory (test-file "degenerate-*.obj")))
    (let* ((mesh (load-mesh (pathname-name file))))
      (finish (normalize (wavefront:vertex-data mesh) (wavefront:index-data mesh))))))

(define-test bounding-sphere
  (is-values (bounding-sphere (f64*))
    (v= 0)
    (= 0))
  (is-values (bounding-sphere (f64* 0 0 0))
    (v= 0)
    (= 0))
  (is-values (bounding-sphere (f64* 1 0 0))
    (v= (dvec 1 0 0))
    (= 0))
  (is-values (bounding-sphere (f64* 1 0 0 -1 0 0))
    (v~= (dvec 0 0 0))
    (~= 1))
  (is-values (bounding-sphere (f64* 1 2 3 -1 2 3))
    (v~= (dvec 0 2 3))
    (~= 1))
  (is-values (bounding-sphere (load-vertices "quad"))
    (v~= (dvec 0 0 0))
    (~= (sqrt 2)))
  (is-values (bounding-sphere (load-vertices "cube"))
    (v~= (dvec 0 0 0))
    (~= (sqrt 3)))
  (skip "Also broken in reference impl"
    (is-values (bounding-sphere (load-vertices "adversarial-cube"))
      (v~= (dvec 0 0 0))
      (~= (sqrt 3))))
  (is-values (bounding-sphere (load-vertices "cospherical-points"))
    (v~= (dvec3 -1.560872e-12 -4.71446e-12 3.675805e-12))
    (~= 1.0))
  (is-values (bounding-sphere (load-vertices "longitude-latitude-model"))
    (v~= (dvec3 1.0952965e-12 7.764871e-12 2.0978248e-16))
    (~= 1.0))
  (is-values (bounding-sphere (load-vertices "random-points"))
    (v~= (dvec 0.01610931 -0.003394038 -0.003313784))
    (~= 0.8286737)))

(defun make-sphere-vertices (count)
  (let ((array (make-array (* count 3) :element-type 'single-float))
        (sample (vec3)))
    (dotimes (i count array)
      (labels ((randn ()
                 (* (sqrt (* -2.0 (log (random 1.0))))
                    (cos (* 2 (float PI 0f0) (random 1.0))))))
        (vsetf sample (randn) (randn) (randn))
        (nv* sample (* (/ (vlength sample)) (expt (random 1.0) 1/3)))
        (replace array (varr sample) :start1 (* i 3))))))

(define-test bounding-sphere.smoke
  (dotimes (i 100)
    (multiple-value-bind (center radius) (bounding-sphere (make-sphere-vertices i))
      (is <= 1.0 (vlength center))
      (is <= 1.0 radius))))
