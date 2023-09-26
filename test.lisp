(defpackage #:org.shirakumo.fraf.manifolds.test
  (:nicknames #:manifolds-test)
  (:use
   #:cl
   #:parachute
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
