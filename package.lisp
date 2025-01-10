(defpackage #:org.shirakumo.fraf.manifolds
  (:use #:cl #:org.shirakumo.fraf.math)
  (:export
   #:vertex-array
   #:face-array
   #:vertex
   #:face
   #:edge
   #:start
   #:end
   #:extended-edge
   #:opposite
   #:u16
   #:u32
   #:f32
   #:f64
   #:ensure-u16
   #:ensure-u32
   #:ensure-f32
   #:ensure-f64
   #:u16*
   #:u32*
   #:f32*
   #:f64*)
  (:export
   #:do-faces
   #:vertex-adjacency-list
   #:face-adjacency-list
   #:half-edge-list
   #:edge-list
   #:adjacent-faces
   #:edge-adjacency-map
   #:boundary-list
   #:v
   #:vertex-normal
   #:face-normal
   #:face-normals
   #:triangle-area
   #:face-edge-p
   #:face-area
   #:surface-area
   #:boundary-length
   #:centroid
   #:volume
   #:closest-point-on-triangle
   #:ray-triangle
   #:face-in-volume-p
   #:faces-in-volume
   #:intersects-volume-p
   #:bounding-box
   #:vertex-faces
   #:2-manifold-p
   #:convex-p
   #:separate-meshes
   #:remove-unused
   #:remove-degenerate-triangles
   #:remove-duplicate-vertices
   #:normalize
   #:transform-mesh
   #:voxelize
   #:bounding-sphere))
