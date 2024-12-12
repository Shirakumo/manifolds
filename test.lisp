(defpackage #:org.shirakumo.fraf.manifolds.test
  (:nicknames #:manifolds-test)
  (:import-from #:org.shirakumo.fraf.math.internal #:~=)
  (:use
   #:cl
   #:parachute
   #:org.shirakumo.fraf.math
   #:org.shirakumo.fraf.manifolds)
  (:local-nicknames
   (#:wavefront #:org.shirakumo.fraf.wavefront)))

(in-package #:org.shirakumo.fraf.manifolds.test)

(defvar *mesh-directory*
  (merge-pathnames
   (make-pathname :directory '(:relative "test"))
   (make-pathname :name nil :type nil :defaults #.(or *compile-file-pathname*
                                                      *load-pathname*))))

(defun load-mesh (name)
  (let* ((filename (merge-pathnames
                    (make-pathname :name name :type "obj")
                    *mesh-directory*))
         (context (wavefront:parse filename))
         (meshes (wavefront:extract-meshes context)))
    (assert (= (length meshes) 1))
    (assert (= (wavefront:face-length (first meshes)) 3))
    (first meshes)))

(defun vertex-array (component-type components)
  (map-into (make-array (length components) :element-type component-type)
            (lambda (value) (coerce value component-type))
            components))

(defun face-array (indices)
  (make-array 3 :element-type 'u32 :initial-contents indices))

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
    (test 'double-float)))

(define-test bounding-sphere
  (is-values (bounding-sphere (f32*))
    (v= 0)
    (= 0))
  (is-values (bounding-sphere (f32* 0 0 0))
    (v= 0)
    (= 0))
  (is-values (bounding-sphere (f32* 1 0 0))
    (v= (vec 1 0 0))
    (= 0))
  (is-values (bounding-sphere (f32* 1 0 0 -1 0 0))
    (v~= (vec 0 0 0))
    (~= 1))
  (is-values (bounding-sphere (f32* 1 2 3 -1 2 3))
    (v~= (vec 0 2 3))
    (~= 1))
  (is-values (bounding-sphere (f32* +1 +1 0  -1 +1 0  +1 -1 0  -1 -1 0))
    (v~= (vec 0 0 0))
    (~= (sqrt 2)))
  (is-values (bounding-sphere (f32* +1 +1 +1  -1 +1 +1  +1 -1 +1  -1 -1 +1
                                    +1 +1 -1  -1 +1 -1  +1 -1 -1  -1 -1 -1))
    (v~= (vec 0 0 0))
    (~= (sqrt 3))))
