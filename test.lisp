(defpackage #:org.shirakumo.fraf.manifolds.test
  (:use #:cl #:parachute #:org.shirakumo.flare.vector)
  (:local-nicknames
   (#:manifolds #:org.shirakumo.fraf.manifolds))
  (:export))

(in-package #:org.shirakumo.fraf.manifolds.test)

(defvar *here* #.(make-pathname :name NIL :type NIL :defaults (or *compile-file-pathname* *load-pathname* (error "COMPILE-FILE or LOAD this."))))

(define-test manifolds)

(defun obj-file (file)
  (merge-pathnames (merge-pathnames file (make-pathname :type "obj" :directory '(:relative "test"))) *here*))

(defun u32 (&rest i)
  (make-array (length i) :element-type '(unsigned-byte 32) :initial-contents i))

(defun f32 (&rest i)
  (make-array (length i) :element-type 'single-float :initial-contents (mapcar #'float i)))

(defun export-hulls (hulls file)
  (org.shirakumo.fraf.wavefront:serialize
   (loop with colors = #(#(1.0 0.0 0.0)
                         #(0.0 1.0 0.0)
                         #(0.0 0.0 1.0)
                         #(0.5 0.5 0.0)
                         #(0.0 0.5 0.5)
                         #(0.5 0.0 0.5)
                         #(1.0 1.0 1.0)
                         #(0.1 0.1 0.1))
         for hull across hulls
         for i from 0
         for mtl = (make-instance 'org.shirakumo.fraf.wavefront:material
                                  :name (format NIL "c~a" i)
                                  :diffuse-factor (aref colors (mod i (length colors))))
         collect (make-instance 'org.shirakumo.fraf.wavefront:mesh
                                :vertex-data (manifolds::convex-hull-vertices hull)
                                :index-data (manifolds::convex-hull-faces hull)
                                :attributes '(:position)
                                :material mtl))
   file :if-exists :supersede)
  hulls)

(defun decompose-hulls (file &rest args)
  (let* ((f (org.shirakumo.fraf.wavefront:parse file))
         (m (first (org.shirakumo.fraf.wavefront:extract-meshes f NIL '(:position)))))
    (apply #'manifolds::decompose
           (org.shirakumo.fraf.wavefront:vertex-data m)
           (org.shirakumo.fraf.wavefront:index-data m)
           args)))

(defun decompose-file (in out &rest args)
  (export-hulls (apply #'decompose-hulls (obj-file in) args)
                (merge-pathnames out (obj-file in))))

(defun decompose-objs (&optional (out (user-homedir-pathname)))
  (dolist (in (directory (test-file (make-pathname :name :wild))))
    (format T "~&> ~a~%" in)
    (%test in (make-pathname :name (pathname-name in) :type "obj" :defaults out)
           :resolution 100)))

(define-test decomposition
  :parent manifolds
  (finish (decompose-hulls (obj-file "box") :resolution 100)))
