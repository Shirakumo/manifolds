#|
 This file is a part of manifolds
 (c) 2023 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.fraf.manifolds
  (:use #:cl #:org.shirakumo.flare.vector)
  (:export
   #:do-faces
   #:vertex-adjacency-list
   #:half-edge-list
   #:edge-list
   #:face-normal
   #:face-normals
   #:closest-point-on-triangle
   #:face-in-volume-p
   #:faces-in-volume
   #:bounding-box
   #:vertex-faces
   #:2-manifold-p
   #:separate-meshes
   #:manifold))
