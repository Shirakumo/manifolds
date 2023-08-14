(defpackage #:org.shirakumo.fraf.manifolds
  (:use #:cl #:org.shirakumo.fraf.math)
  (:export
   #:u32
   #:f32
   #:f64
   #:do-faces
   #:vertex-adjacency-list
   #:face-adjacency-list
   #:half-edge-list
   #:edge-list
   #:boundary-list
   #:v
   #:face-normal
   #:face-normals
   #:face-area
   #:surface-area
   #:boundary-length
   #:centroid
   #:convex-volume
   #:closest-point-on-triangle
   #:face-in-volume-p
   #:faces-in-volume
   #:bounding-box
   #:vertex-faces
   #:2-manifold-p
   #:separate-meshes))
